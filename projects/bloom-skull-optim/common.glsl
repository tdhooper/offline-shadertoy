precision highp float;

#define DISABLE_DOF
#define DISABLE_SHADOWS

float loopTime(float iTime) {
	return mod(iTime / 3. + .35, 1.);
}

uniform float iTime;

// voxel resolution is
// vec3(
//     iResolution / texSubdivisions,
//     texSubdivisions.x * texSubdivisions.y * 4.
// );
vec2 texSubdivisions = vec2(13,6);

#define MIRROR
#define SCALE (vec3(4.1,1.8,1.75))
#define OFFSET vec3(.95, .05, .05)
