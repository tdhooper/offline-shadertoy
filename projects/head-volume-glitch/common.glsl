precision highp float;

uniform vec2 iResolution;
uniform float iTime;

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

#define SCALE (vec3(3.5,1.73,1.75) * .5)
#define OFFSET vec3(0, 0, .05)



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
float round(float t) { return floor(t + 0.5); }
vec2 round(vec2 t) { return floor(t + 0.5); }

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


void pR2(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}


vec2 gA(vec2 coord) {
    return coord * mix(1., tan(coord.y*10./coord.x*5.-iTime * 3.), .04 / 100.);
}

vec2 gB(vec2 coord) {
    return coord * mix(1., tan(coord.x*10./coord.y*5.-iTime * 3.), .02 / 100.);
}

vec2 gC(vec2 coord) {
    return coord * mix(1., tan(coord.y/10.-iTime * 1.), .002);
}

vec2 gD(vec2 coord) {
    return coord * mix(1., sin(coord.x/coord.y*50.-iTime * 3.), .001);
}

void gate(inout float t) {
    t = smoothstep(.4, .5, t);
}

vec2 glitchCoords(vec2 coord) {
   // return coord * mix(1., tan(coord.y*10./coord.x*5.+iTime * 3.), .04 / 100.);


    float tm = iTime * 3.;
    tm = 2.;
    float t1 = sin(tm) * .5 + .5;
    float t2 = cos(tm * 1.5) * .5 + .5;
    float t3 = sin(tm * 2.) * .5 + .5;
    float t4 = cos(tm * 2.5) * .5 + .5;
    gate(t1); gate(t2); gate(t4);
    coord = mix(coord, gA(coord), t1);
    coord = mix(coord, gB(coord), t2);
    coord = mix(coord, gC(coord), t3);
    coord = mix(coord, gD(coord), t4);
    return coord;
}

// Transform xyz coordinate in range -1,-1,-1 to 1,1,1
// to texture uv and channel
vec3 spaceToTex(vec3 p, vec2 size) {

    p = clamp(p, -1., 1.);

    // p.x = mix(0., sin(p.x * 2.) * 10., .1);
    
    // p *= 1.3;
    // p.z -= .3;

    p = p * .5 + .5; // range 0:1

    // p.x = sin(p.x * 5.) / 2.;

    vec2 sub = texSubdivisions;
    vec2 subSize = floor(size / sub);

    // uv = clamp(uv, 0., 1.);

    // Work out the z index
    float zRange = sub.x * sub.y * 4. - 1.;

    float i = round(p.z * zRange);

    //return vec3(i/zRange);

    // return vec3(mod(i, sub.x)/sub.x);
    // translate uv into the micro offset in the z block
    vec2 coord = p.xy * subSize;

    // Work out the macro offset for the xy block from the z block
    coord += vec2(
        mod(i, sub.x),
        mod(floor(i / sub.x), sub.y)
    ) * subSize;

    coord = glitchCoords(coord);

    float c = floor(i / (sub.x * sub.y));
    vec3 uvc = vec3(coord / size, c);


    float f = 1500.;
    uvc.xy = mix(uvc.xy, round(uvc.xy * vec2(f)) / vec2(f), .5);
    // pR2(uvc.xy, .015);

    return uvc;
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

float mapTex(sampler2D tex, vec3 p, vec2 size) {
    p = p;
    // stop x bleeding into the next cell as it's the mirror cut
    #ifdef MIRROR
        p.x = clamp(p.x, -.95, .95);
    #endif
    vec2 sub = texSubdivisions;
    float zRange = sub.x * sub.y * 4. - 1.;
    float z = p.z * .5 + .5; // range 0:1
    float zFloor = (floor(z * zRange) / zRange) * 2. - 1.;
    float zCeil = (ceil(z * zRange) / zRange) * 2. - 1.;
    vec3 uvcA = spaceToTex(vec3(p.xy, zFloor), size);
    vec3 uvcB = spaceToTex(vec3(p.xy, zCeil), size);
    float a = pickIndex(texture2D(tex, uvcA.xy), int(uvcA.z));
    float b = pickIndex(texture2D(tex, uvcB.xy), int(uvcB.z));
    // return a;
    return mix(a, b, range(zFloor, zCeil, p.z));
}

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

