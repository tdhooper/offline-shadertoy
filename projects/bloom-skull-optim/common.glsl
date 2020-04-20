precision highp float;

//#define DISABLE_DOF
#define DISABLE_SHADOWS

float loopTime(float iTime) {
	return mod(iTime / 3. + .35, 1.);
}

uniform float iTime;
float mTime = mod(iTime / 1., 1.);

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
