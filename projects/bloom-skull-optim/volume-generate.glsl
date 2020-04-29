// framebuffer size: 4096x4096 firstpassonly

uniform vec2 iResolution;
uniform vec2 iChannel0Size;
uniform bool firstPass;

#pragma glslify: sdSkull = require(../skull/skull.glsl)
#pragma glslify: texToSpace = require(./volume-write.glsl)

float map(vec3 p) {
    p -= OFFSET;
    p /= SCALE;
    return sdSkull(p);
    // return fBox(p, vec3(.5));
    //return length(p) - .4;
}

void main() {
    vec2 coord = gl_FragCoord.xy;
    vec2 size = iResolution;
    vec2 uv = coord / size;

    mat4 space = texToSpace(coord, size);

    vec3 p0 = space[0].xyz;
    vec3 p1 = space[1].xyz;
    vec3 p2 = space[2].xyz;
    vec3 p3 = space[3].xyz;

    gl_FragColor = vec4(
        map(p0),
        map(p1),
        map(p2),
        map(p3)
    );
}
