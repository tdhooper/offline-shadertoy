#define SHADER_NAME quad.vert

attribute vec2 position;
void main() {
    gl_Position = vec4(2.0 * position - 1.0, 0, 1);
}
