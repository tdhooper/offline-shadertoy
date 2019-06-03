precision highp float;

uniform vec2 iResolution;

// 640 x 360 buffer
// 40 x 180 x 128 voxels
vec2 texSubdivisions = vec2(16,2);

#define SCALE (vec3(4.1,1.73,1.75) * 1.)
#define OFFSET vec3(.95, .094, -.088)


// Divide texture into 3d space coordinates
// uv = 2d texture coordinates 0:1
// c = channel 0:3

// xy is split for each z

// Returns matrix representing three positions in space
// vec3 p0 = mat4[0].xyz;
// vec3 p1 = mat4[1].xyz;
// vec3 p2 = mat4[2].xyz;
// vec3 p3 = mat4[4].xyz;

vec3 texToSpace(vec2 uv, int c) {
    vec2 sub = texSubdivisions;
    uv *= sub;
    float z = floor(uv.x) + floor(uv.y) * sub.x + float(c) * sub.x * sub.y;
    z /= sub.x * sub.y * 4. - 1.;
    uv = mod(uv, 1.);
    vec3 p = vec3(uv, z);
    p = p * 2. - 1.; // range -1:1
    return p;
}

mat4 texToSpace(vec2 uv) {
    return mat4(
        vec4(texToSpace(uv, 0), 0),
        vec4(texToSpace(uv, 1), 0),
        vec4(texToSpace(uv, 2), 0),
        vec4(texToSpace(uv, 3), 0)
    );
}


// uv and channel
vec3 spaceToTex(vec3 p) {
    p = clamp(p, -1., 1.);
    p = p * .5 + .5; // range 0:1
    vec2 sub = texSubdivisions;
    vec2 uv = p.xy;
    //uv = clamp(uv, 0., 1.);
    uv /= sub;
    float i = floor(p.z * (sub.x * sub.y * 4. - 1.));
    uv += vec2(mod(i, sub.x), mod(floor(i / sub.x), sub.y)) / sub;
    float c = floor(i / (sub.x * sub.y));
    return vec3(uv, c);
}

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float pickIndex(vec4 v, int i) {
    if (i == 0) return v.r;
    if (i == 1) return v.g;
    if (i == 2) return v.b;
    if (i == 3) return v.a;
}

float mapTex(sampler2D tex, vec3 p) {
    // stop x bleeding into the next cell as it's the mirror cut
    p.x = clamp(p.x, -.95, .95);
    vec2 sub = texSubdivisions;
    float zRange = (sub.x * sub.y * 4. - 1.) / 2.;
    p.z += .5/zRange;
    float zFloor = floor(p.z * zRange) / zRange;
    float zCeil = ceil(p.z * zRange) / zRange;
    vec3 uvcA = spaceToTex(vec3(p.xy, zFloor));
    vec3 uvcB = spaceToTex(vec3(p.xy, zCeil));
    float a = pickIndex(texture2D(tex, uvcA.xy), int(uvcA.z));
    float b = pickIndex(texture2D(tex, uvcB.xy), int(uvcB.z));
    return mix(a, b, range(zFloor, zCeil, p.z));
}

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

