precision mediump float;
attribute vec2 position;
uniform mat4 projection;
uniform mat4 view;
varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;

void main() {
    vec2 vertex = 2.0 * position - 1.0;
    gl_Position = vec4(vertex, 0, 1);

    float fov = 1. / projection[1].y;
    float aspect = projection[1].y / projection[0].x;
    eye = -(view[3].xyz) * mat3(view);
    dir = vec3(vertex.x * fov * aspect, vertex.y * fov,-1.0) * mat3(view);
    cameraForward = vec3(0,0,-1) * mat3(view);
}
