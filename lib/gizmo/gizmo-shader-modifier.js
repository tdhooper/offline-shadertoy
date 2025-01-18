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

let addIndiciesToTransformMethodCalls = (source) => {
  let reMethodCall = new RegExp("gmTransform[\\s\\n]*\\(([^{;]*);", "gd");
  let match;
  let positions = [];
  while((match = reMethodCall.exec(source)) !== null) {
    positions.push(match.indices[1][0]);
  }
  const count = positions.length;
  if (count > 0) {
    positions.reverse();
    positions.forEach((position, index) => {
      source = source.slice(0, position) + `${count - index - 1}, ` + source.slice(position);
    });
  }
  return { source, count };
}

let headerInsertionIndex = (source) => {
  let reVersion = new RegExp("^#version.*", "g");
  let insertIndex = 0;
  if (reVersion.test(source)) {
    insertIndex = reVersion.lastIndex;
  }
  return insertIndex;
}

let addTransformMethodIfMissing = (source) => {
  let updated = false;
  if (source.indexOf('float gmTransform(') == -1) {
    let insertIndex = headerInsertionIndex(source);
    let insert = `
precision mediump float;

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
  p -= t;
  p = mix(dot(r.xyz,p)*r.xyz, p, cos(-r.w))+sin(-r.w)*cross(r.xyz,p);
  p /= s;
  return min(s.x, min(s.y, s.z));
}
`;
    source = source.slice(0, insertIndex) + insert + source.slice(insertIndex);
    updated = true;
  }
  return { source, updated };
}

let makeTransformMethodAdjustable = (source, count) => {
  source = renameMethod(source, 'float', 'gmTransform', 'NON_ADJUSTABLE_gmTransform');
  let insertIndex = headerInsertionIndex(source);
  let insert = `
precision mediump float;

struct GizmoTransform {
  vec3 t;
  vec4 r;
  vec3 s;
};

uniform bool gizmoAdjust[${count}];
uniform GizmoTransform gizmoAdjustment[${count}];

float NON_ADJUSTABLE_gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s);

float gmTransform(int index, inout vec3 p, vec3 t, vec4 r, vec3 s) {
    float scale;
    if (gizmoAdjust[index]) {
        scale = NON_ADJUSTABLE_gmTransform(p, gizmoAdjustment[index].t, gizmoAdjustment[index].r, gizmoAdjustment[index].s);
    } else {
        scale = NON_ADJUSTABLE_gmTransform(p, t, r, s);
    }
    return scale;
}

float gmTransform(int index, inout vec3 p) {
    float scale = 1.;
    if (gizmoAdjust[index]) {
        return NON_ADJUSTABLE_gmTransform(p, gizmoAdjustment[index].t, gizmoAdjustment[index].r, gizmoAdjustment[index].s);
    }
    return 1.;
}
`;
  source = source.slice(0, insertIndex) + insert + source.slice(insertIndex);
  return { source };
}

let exposeArgumentsFromTransformMethod = (source, count) => {
  source = renameMethod(source, 'float', 'gmTransform', 'NON_EXPOSED_gmTransform');
  let insertIndex = headerInsertionIndex(source);
  let insert = `
precision mediump float;

struct GizmoTransformArguments {
  vec3 p;
  vec3 t;
  vec4 r;
  vec3 s;
};

GizmoTransformArguments GIZMO_ARGUMENTS[${count}];
bool GIZMO_ARGUMENTS_SET[${count}];

float NON_EXPOSED_gmTransform(int index, inout vec3 p, vec3 t, vec4 r, vec3 s);
float NON_EXPOSED_gmTransform(int index, inout vec3 p);

float gmTransform(int index, inout vec3 p, vec3 t, vec4 r, vec3 s) {
    if ( ! GIZMO_ARGUMENTS_SET[index]) {
        GIZMO_ARGUMENTS[index] = GizmoTransformArguments(p, t, r, s);
        GIZMO_ARGUMENTS_SET[index] = true;
    }
    return NON_EXPOSED_gmTransform(index, p, t, r, s);
}

float gmTransform(int index, inout vec3 p) {
    if ( ! GIZMO_ARGUMENTS_SET[index]) {
        GIZMO_ARGUMENTS[index] = GizmoTransformArguments(p, vec3(0, 0, 0), vec4(1, 0, 0, 0), vec3(1, 1, 1));
        GIZMO_ARGUMENTS_SET[index] = true;
    }
    return NON_EXPOSED_gmTransform(index, p);
}
`;
  source = source.slice(0, insertIndex) + insert + source.slice(insertIndex);
  return { source };
}

let rewriteMainForTransformFinder = (source, count) => {
  let reFragColor = new RegExp("out\\s+vec4\\s+([^;\\s]+)", "g");
  let fragColorMatch = reFragColor.exec(source);
  if ( ! fragColorMatch) {
    console.error('Gizmo could not could not find out fragColor variable name');
    return { source };
  }
  const fragColor = fragColorMatch[1];

  source = renameMethod(source, 'void', 'main', 'GIZMO_MAIN');
  let hasDepth = source.indexOf('gl_FragDepthEXT') != -1;
  source += `
uniform vec2 evalGizmoPositionsResolution;
uniform sampler2D evalGizmoPositions;
void main() {
  GIZMO_MAIN();
  vec3 position = texelFetch(evalGizmoPositions, ivec2(gl_FragCoord.x, 0), 0).rgb;
  GIZMO_ARGUMENTS_SET = bool[${count}](${(new Array(count)).fill('false').join(', ')});
  GIZMO_MAP(position);

  int index = int(gl_FragCoord.y);

  if (gl_FragCoord.x < 1.) {
    ${fragColor} = vec4(GIZMO_ARGUMENTS[index].t, 0);
  } else if (gl_FragCoord.x < 2.) {
    ${fragColor} = GIZMO_ARGUMENTS[index].r;
  } else if (gl_FragCoord.x < 3.) {
    ${fragColor} = vec4(GIZMO_ARGUMENTS[index].s, 0);
  } else {
    ${fragColor} = vec4(GIZMO_ARGUMENTS[index].p, 0);
  }
  
  ${ hasDepth ? 'gl_FragDepthEXT = 0.;' : '' }
}
  `;
  return { source };
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
  let reMethodCall = new RegExp("gmTransform[\\s\\n]*\\(([^,;\\)]+)[^;{]*;", "gd");
  let match;
  let segments = [];
  while((match = reMethodCall.exec(source)) !== null) {
    let start = match.indices[1][1];
    let end = findClosingBracket(source, start);
    segments.push({start, end});
  }
  return segments;
}

let replaceTransformCallArguments = (source, newArgsList) => {
  let updated = false;
  let segments = findTransformCallArgumentSegments(source);
  const count = segments.length;
  if (count > 0) {
    segments.reverse();
    segments.forEach((segment, index) => {
      let newArgs = newArgsList[count - index - 1];
      if (newArgs) {
        const insert = `, ${newArgs}`;
        source = source.slice(0, segment.start) + insert + source.slice(segment.end);
        updated = true;
      }
    });
  }
  return { source, updated };
}

export default {
  usesTransformMethod,
  addIndiciesToTransformMethodCalls,
  addTransformMethodIfMissing,
  makeTransformMethodAdjustable,
  exposeArgumentsFromTransformMethod,
  rewriteMainForTransformFinder,
  replaceTransformCallArguments,
};
