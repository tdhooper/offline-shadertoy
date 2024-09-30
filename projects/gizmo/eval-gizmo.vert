precision mediump float;
uniform float count;
attribute vec4 position;
varying vec3 vPosition;

void main() {
    float x = (position.w + .5) / count;
    x = x * 2. - 1.;
    gl_Position = vec4(x, 0, 0, 1);
    gl_PointSize = 1.;
    vPosition = position.xyz;
}
