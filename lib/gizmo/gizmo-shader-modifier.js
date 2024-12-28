
let renameMethod = (source, returnType, methodName, newMethodName) => {
  let reMethod = new RegExp(returnType + "[\\s\\n]+("+methodName+")[\\s\\n]*\\([^\\(\\)]*\\)[\\s\\n]*{", "gd");
  let match;
  let segments = [];
  while((match = reMethod.exec(source)) !== null) {
    let start = match.indices[1][0];
    let end = match.indices[1][1];
    segments.push({start, end});
  }
  if (segments.length > 0) {
    segments.reverse();
    segments.forEach(segment => {
      source = source.slice(0, segment.start) + newMethodName + source.slice(segment.end);
    });
  }
  return source;
};

let usesTransformMethod = (source) => {
  return source.indexOf('gmTransform(') != -1;
}

let addTransformMethodIfMissing = (source) => {
  let updated = false;
  if (source.indexOf('float gmTransform(') == -1) {
    source = `
precision mediump float;

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
  p -= t;
  p = mix(dot(r.xyz,p)*r.xyz, p, cos(-r.w))+sin(-r.w)*cross(r.xyz,p);
  p /= s;
  return min(s.x, min(s.y, s.z));
}
` + source;
    updated = true;
  }
  return { source, updated };
}

let makeTransformMethodAdjustable = (source) => {
  source = renameMethod(source, 'float', 'gmTransform', 'NON_ADJUSTABLE_gmTransform');
  source = `
precision mediump float;

struct GizmoTransform {
  vec3 t;
  vec4 r;
  vec3 s;
};

uniform bool gizmoAdjust;
uniform GizmoTransform gizmoAdjustment;

float NON_ADJUSTABLE_gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s);

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
    float scale;
    if (gizmoAdjust) {
        scale = NON_ADJUSTABLE_gmTransform(p, gizmoAdjustment.t, gizmoAdjustment.r, gizmoAdjustment.s);
    } else {
        scale = NON_ADJUSTABLE_gmTransform(p, t, r, s);
    }
    return scale;
}

float gmTransform(inout vec3 p) {
    float scale = NON_ADJUSTABLE_gmTransform(p, gizmoAdjustment.t, gizmoAdjustment.r, gizmoAdjustment.s);
    return scale;
}
` + source;
  return source;
}

let exposeArgumentsFromTransformMethod = (source) => {
  source = renameMethod(source, 'float', 'gmTransform', 'NON_EXPOSED_gmTransform');
  source = `
precision mediump float;

struct GizmoTransformArguments {
  vec3 p;
  vec3 t;
  vec4 r;
  vec3 s;
};

GizmoTransformArguments GIZMO_ARGUMENTS;
bool GIZMO_ARGUMENTS_SET = false;

float NON_EXPOSED_gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s);
float NON_EXPOSED_gmTransform(inout vec3 p);

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
    if ( ! GIZMO_ARGUMENTS_SET) {
      GIZMO_ARGUMENTS = GizmoTransformArguments(p, t, r, s);
      GIZMO_ARGUMENTS_SET = true;
    }
    return NON_EXPOSED_gmTransform(p, t, r, s);
}

float gmTransform(inout vec3 p) {
    if ( ! GIZMO_ARGUMENTS_SET) {
      GIZMO_ARGUMENTS = GizmoTransformArguments(p, vec3(0, 0, 0), vec4(1, 0, 0, 0), vec3(1, 1, 1));
      GIZMO_ARGUMENTS_SET = true;
    }
    return NON_EXPOSED_gmTransform(p);
}
` + source;
  return source;
}

let rewriteMainForTransformFinder = (source) => {
  source = renameMethod(source, 'void', 'main', 'GIZMO_MAIN');
  let hasDepth = source.indexOf('gl_FragDepthEXT') != -1;
  source += `
uniform vec2 evalGizmoPositionsResolution;
uniform sampler2D evalGizmoPositions;
void main() {
  GIZMO_MAIN();
  vec3 position = texture2D(evalGizmoPositions, vec2(gl_FragCoord.x, gl_FragCoord.y) / evalGizmoPositionsResolution).rgb;
  GIZMO_ARGUMENTS_SET = false;
  GIZMO_MAP(position);

  if (gl_FragCoord.x < 1.) {
    gl_FragColor = vec4(GIZMO_ARGUMENTS.t, 0);
  } else if (gl_FragCoord.x < 2.) {
    gl_FragColor = GIZMO_ARGUMENTS.r;
  } else if (gl_FragCoord.x < 3.) {
    gl_FragColor = vec4(GIZMO_ARGUMENTS.s, 0);
  } else {
    gl_FragColor = vec4(GIZMO_ARGUMENTS.p, 0);
  }
  
  ${ hasDepth ? 'gl_FragDepthEXT = 0.;' : '' }
}
  `;
  return source;
}

// this doesn't cope with comments or macros
let findClosingBracket = (source, startIndex) => {
  let counter = 0;
  let index = startIndex;
  while (index < source.length) {
    let char = source.charAt(index);
    if (char == "(") {
      counter++;
    }
    if (char == ")") {
      counter--;
    }
    if (counter < 0) {
      return index;
    }
    index++;
  }
  return startIndex;
}

let findTransformCallArgumentSegments = (source) => {
  let reMethodCall = new RegExp("gmTransform[\\s\\n]*\\(([^,])[^;{]*;", "gd");
  let match;
  let segments = [];
  while((match = reMethodCall.exec(source)) !== null) {
    let start = match.indices[1][1];
    let end = findClosingBracket(source, start);
    segments.push({start, end});
  }
  return segments;
}

let replaceTransformCallArguments = (source, newArgs) => {
  let updated = false;
  let segments = findTransformCallArgumentSegments(source);
  if (segments.length > 0) {
    segments.reverse();
    segments.forEach(segment => {
      const insert = `, ${newArgs}`;
      source = source.slice(0, segment.start) + insert + source.slice(segment.end);
    });
    updated = true;
  }
  return { source, updated };
}

export default {
  usesTransformMethod,
  addTransformMethodIfMissing,
  makeTransformMethodAdjustable,
  exposeArgumentsFromTransformMethod,
  rewriteMainForTransformFinder,
  replaceTransformCallArguments,
};
