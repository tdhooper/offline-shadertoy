precision mediump float;
attribute vec2 position;

void main() {
    vec2 vertex = 2.0 * position - 1.0;
    gl_Position = vec4(vertex, 0, 1);
}
