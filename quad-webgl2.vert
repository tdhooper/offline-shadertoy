#version 300 es

precision mediump float;

in vec2 position;

uniform mat4 projection;
uniform mat4 view;

out vec3 eye;
out vec3 dir;
out vec3 cameraForward;
out float fov;
out float aspect;
out mat4 vView;
out vec2 vVertex;

void main() {
    vec2 vertex = 2.0 * position - 1.0;
    gl_Position = vec4(vertex, 0, 1);
    vVertex = vertex;
    vView = view;
    fov = 1. / projection[1].y;
    aspect = projection[1].y / projection[0].x;
    eye = -(view[3].xyz) * mat3(view);
    dir = vec3(vertex.x * fov * aspect, vertex.y * fov,-1.0) * mat3(view);
    cameraForward = vec3(0,0,-1) * mat3(view);
}
