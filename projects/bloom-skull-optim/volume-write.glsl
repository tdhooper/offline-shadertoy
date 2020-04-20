


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

vec3 texToSpace1(vec2 coord, int c, vec2 size) {
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
        vec4(texToSpace1(coord, 0, size), 0),
        vec4(texToSpace1(coord, 1, size), 0),
        vec4(texToSpace1(coord, 2, size), 0),
        vec4(texToSpace1(coord, 3, size), 0)
    );
}

#pragma glslify: export(texToSpace)
