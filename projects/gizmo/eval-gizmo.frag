precision mediump float;
varying vec3 vPosition;

#pragma glslify: map = require(./map.glsl,GIZMO_LOCAL_P=GIZMO_LOCAL_P)

void main() {
  map(vPosition);
  gl_FragColor = vec4(GIZMO_LOCAL_P, 0);
  //gl_FragColor = vec4(vPosition, 0);
}
