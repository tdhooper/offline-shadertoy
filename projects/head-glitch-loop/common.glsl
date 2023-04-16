precision highp float;

uniform vec2 iResolution;
uniform float iTime;

#define PI 3.14159265359

// #define MIRROR

//vec2 texSubdivisions = vec2(5,5);
//vec2 texSubdivisions = vec2(6,3);
vec2 texSubdivisions = vec2(10,3);
// voxel resolution is
// vec3(
//     iResolution / texSubdivisions,
//     texSubdivisions.x * texSubdivisions.y * 4.
// );
//

// #define SCALE (vec3(4.1,1.73,1.75) * 1.)
#define SCALE (vec3(4.1/2.,1.73,1.75) * .6)

// #define OFFSET vec3(.95, .094, -.088)
#define OFFSET vec3(0, .094, -.1)



// #define SCALE vec3(1)
// #define OFFSET vec3(0)


// Divide texture into 3d space coordinates
// uv = 2d texture coordinates 0:1
// c = channel 0:3

// xy is split for each z

// Returns matrix representing four positions in space
// vec3 p0 = mat4[0].xyz;
// vec3 p1 = mat4[1].xyz;
// vec3 p2 = mat4[2].xyz;
// vec3 p3 = mat4[4].xyz;

// current theory its rounding errors when texture doesn't divide

vec3 texToSpace(vec2 coord, int c, vec2 size) {
    vec2 sub = texSubdivisions;
    vec2 subSize = floor(size / sub);
    vec2 subCoord = floor(coord / subSize);
    float z = subCoord.x + subCoord.y * sub.x + float(c) * sub.x * sub.y;
    float zRange = sub.x * sub.y * 4. - 1.;
    z /= zRange;
    vec2 subUv = mod(coord / subSize, 1.);
    vec3 p = vec3(subUv, z);
    p = p * 2. - 1.; // range -1:1
    return p;
}

mat4 texToSpace(vec2 coord, vec2 size) {
    return mat4(
        vec4(texToSpace(coord, 0, size), 0),
        vec4(texToSpace(coord, 1, size), 0),
        vec4(texToSpace(coord, 2, size), 0),
        vec4(texToSpace(coord, 3, size), 0)
    );
}

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

