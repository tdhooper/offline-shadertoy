precision mediump float;
uniform vec2 evalGizmoPositionsResolution;
uniform sampler2D evalGizmoPositions;

#pragma glslify: map = require(./map.glsl,GIZMO_LOCAL_P=GIZMO_LOCAL_P,GIZMO_LOCAL_P2=GIZMO_LOCAL_P2)

void main() {
  vec3 position = texture2D(evalGizmoPositions, vec2(gl_FragCoord.x, gl_FragCoord.y) / evalGizmoPositionsResolution).rgb;
  map(position);
  if (gl_FragCoord.y > 1.) {
    gl_FragColor = vec4(GIZMO_LOCAL_P, 0);
  } else {
    gl_FragColor = vec4(GIZMO_LOCAL_P2, 0);
  }
  //gl_FragColor = vec4(gl_FragCoord.x, gl_FragCoord.y, 0, 0);
  //gl_FragColor = vec4(position, 0);
}
