precision highp float;

uniform float iTime;
float mTime = mod(iTime / 6., 1.);

#define MIRROR

vec2 texSubdivisions = vec2(13,6);
// voxel resolution is
// vec3(
//     iResolution / texSubdivisions,
//     texSubdivisions.x * texSubdivisions.y * 4.
// );
//

#define SCALE (vec3(4.1,1.73,1.75))
#define OFFSET vec3(.95, 0, .05)

// #define SCALE vec3(1)
// #define OFFSET vec3(0)
