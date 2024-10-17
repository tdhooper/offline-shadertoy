uniform mat4 gizmoAdjustmentMatrix;

vec3 GIZMO_LOCAL_P;
vec3 GIZMO_LOCAL_P2;

void GIZMO(inout vec3 p, mat4 m) {
    GIZMO_LOCAL_P = p;
    p = (m * vec4(p, 1)).xyz;
    GIZMO_LOCAL_P2 = p;
    p = (gizmoAdjustmentMatrix * vec4(p, 1)).xyz;
}

void GIZMO(inout vec3 p) {
    GIZMO_LOCAL_P = p;
    GIZMO_LOCAL_P2 = p;
    p = (gizmoAdjustmentMatrix * vec4(p, 1)).xyz;
}



#define PI 3.1415926

// HG_SDF

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}
float vmax2(vec2 v) {
	return max(v.x, v.y);
}
float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}
float vmin2(vec2 v) {
	return min(v.x, v.y);
}
float sum(vec3 v) {
    return v.x + v.y + v.z;
}
float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
	float t = dot(p, planeNormal)+offset;
	if (t < 0.) {
		p = p - (2.*t)*planeNormal;
	}
	return sign(t);
}
float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}
float fBox2(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax2(min(d, vec2(0)));
}
float fHalfCapsule(vec2 p, float r) {
    p.y = max(p.y, 0.);
    return length(p) - r;
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}
float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

// Rotate on axis, blackle https://suricrasia.online/demoscene/functions/
vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(ax,p)*ax, p, cos(ro))+sin(ro)*cross(ax,p);
}


float unlerp(float vmin, float vmax, float value) {
    return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float smoothbump(float a, float b, float x) {
    float c = mix(a, b, .5);
    return smoothstep(a, c, x) - smoothstep(c, b, x);
}



//========================================================
// Noise
//========================================================

// https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}


// https://www.shadertoy.com/view/4djSRW
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

vec3 hash31(float p)
{
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); 
}

// https://suricrasia.online/demoscene/functions/
vec3 rndunit(float seed) {
  return normalize(tan(hash31(seed)));
}


vec3 sofasz = vec3(.2, .1, .1) / 2.;
    
float fSofa(vec3 p) {
    
    p += sum(sin(erot(p, vec3(1), 1.) * 6000.)) * .000015;
    
    float fade, d2, d3, d4, ar, armang, ar0, ar1, arw, footh, psx, vary, baseh, cs, cr, axisx, axisz, crw, seam, buttpatch, br;
    vec2 armtopp;
    vec3 isofasz, pp, armsz, pc, col, backsz, cushionsz;

    armsz = vec3(.012,.026,sofasz.z);
    footh = .0075;
    isofasz = sofasz - vec3(armsz.x * 2., 0, 0);

    psx = sign(p.x);
    vary = max(psx, 0.);
    pp = p;

    // arms
    p.x = abs(p.x);
    p.x -= sofasz.x - armsz.x;
    p.y -= footh - sofasz.y + armsz.y;

    d2 = fHalfCapsule(p.xy - vec2(0, armsz.y), armsz.x);
    armtopp = p.xy - vec2(.004, armsz.y + .005);
    d2 = smin(d2, length(armtopp) - armsz.x - .002, .01);
    d3 = d2 + armsz.x * .4;
    ar = .007;
    armang = atan(armtopp.y, armtopp.x);
    ar0 = sin(sin(sin(armang) * 10. - vary*2.) * 5. + length(armtopp) * 500. + p.z * 100.);
    ar1 = sin(sin(sin((p.y + 1.) * 65.) * 10.) * 5. + p.x * 300. + p.z * 100.);
    arw = mix(ar0, ar1, smoothstep(.01, -.01, p.y - armsz.y / 3.));

    arw *= smoothstep(0., armsz.z, abs(p.z));
    fade = mix(.33, 0., arw) * smoothstep(.01, .0, length(vec2(d2, 2. * abs(abs(p.z) - armsz.z + .0035)))) * smoothstep(-.03, .06, p.y);// mix(1., 0., arw) // * mix(.5, 1., smoothstep(.005, 0., abs(abs(p.z) - armsz.z + .005)));
    d2 = smax(d2 + arw * .0001, abs(p.z) - armsz.z, ar + arw * .0005);
    d3 = max(d3, -p.z);
    if (d3 > 0.) {
        fade = max(fade, smoothbump(.5, 3., mod(armang - .5, PI * 2.)) * smoothbump(-armsz.z / 2., armsz.z * 1.5, p.z)) *  mix(1., .5, arw);
    }
    ar = .00725;
    d4 = smax(d3, abs(p.z) - armsz.z - .003, ar);
    d2 = smax(d2, -d4, .001);
    ar += sin(sin(sin((p.y + 1.) * 80.) * 10.) * 5. + p.x * 300.) * .0003;
    d4 = smax(d3, abs(p.z) - armsz.z - .003, ar);
    d2 = min(d2, d4);

    d2 = smax(d2, -p.y - armsz.y, .003);
	

    // base
    p = pp;
    baseh = .012;
    p.y -= footh - sofasz.y + baseh;
    br = .002;
    br += sin(sin(sin(p.x * 55. + 2.) * 10.) * 5. + p.y * 100.) * .00025;
	
    d3 = fBox(p, vec3(isofasz.x + .0068, baseh, isofasz.z) - br) - br;

    d2 = min(d2, d3);
    p.x = abs(p.x);
    p.xy -= vec2(sofasz.x / 2.5, 0.);
    p.y += sin(p.x * 250.) * .0005;
    p.xy *= vec2(.4, 1);
    fade = max(fade, pow(smoothstep(.025, .0, length(p.xy)), 3.)/2.);

	/*
    // back
    p = pp;
    backsz = vec3(isofasz.x + .001, armsz.y + .005, .005);
    p.y -= footh - sofasz.y + baseh + backsz.y;
    p.z -= -sofasz.z + backsz.z;
    d3 = fBox(p.zyx, backsz.zyx - .006) - .006;
    d2 = min(d2, d3);
	*/
    // cushion
    p = pp;
    
    p.x = abs(p.x);
    
cushionsz = vec3(isofasz.x / 2. + .001, .01, sofasz.z - .018);
    p.y -= footh - sofasz.y + baseh * 2. - .001;
    p.y -= cushionsz.y * 2.;
    p.x -= isofasz.x / 2.;
    p.z -= sofasz.z - cushionsz.z * 2. + .005;
    vary += pReflect(p, normalize(vec3(0,-.66,1)), 0.);
        GIZMO(p);
    p.y += cushionsz.y;
    p.z -= cushionsz.z - .004;

    cs = mix(.95, 1.01, length(sin(sin((p + vary * 240.) * 30.) * 3.) * .5 + .5));
    cs = 1. + sum(sin((p + vary * 2. + 1.) * 100.) * vec3(1,0,1)) * .02;
    cr = .008;
    axisx = max(vmin2(p.xz), vmin2(-p.xz));
    axisz = min(vmin2(p.xz), vmin2(-p.xz));
		
    crw = sin(sin(sin((axisx + mix(.4, .6, vary)) * (48. + vary)) * 10.) * 5. + p.y * 300. + p.z * 300.);
    crw *= smoothstep(0., .0005, abs(dot(abs(p.xz), cushionsz.zx * vec2(1,-1))));
    crw *= unlerp(.5, 1., vmax2(abs(p.xz) / cushionsz.xz));
    cr += crw * .0003;

    cs += smoothstep(cushionsz.x * 1.2, 0., length(p.xz)) * .4;
    pc = p;
    pc.x += sin(pc.z * 150. + vary * 3.) * .02;
    pR(pc.xz, -vary * .2 + 1.6);
    buttpatch = sum(sin((sin(2. + vec3(pc.x, pc.y, 0) * 122. * (1. + abs(vary - 1.) * .2)) + vary * 30.) * 8.));
    
    buttpatch *= smoothstep(cushionsz.z, 0., length(p.xz));
    cs += buttpatch * .03;

    d3 = (fBox(p / cs, cushionsz - cr - crw * .0001) - cr) * cs* .9;
    seam = abs(p.y) - cushionsz.y * .75;
    d3 = smax(d3, -abs(seam), .001);

    if (d3 < d2) {
        d2 = d3;
        fade = 0.;
        fade += smoothbump(.0, .0015, seam) * mix(.5, .25, crw);
        fade += smoothbump(.0, .00075, -seam) * mix(.5, .25, crw);
        fade = max(fade, smoothstep(cushionsz.x * 1.1, 0., length(p.xz)));
        if (psx < 0.) {
            fade /= 3.;
        } else {
            fade *= 1.2;
            fade += .1;
        }
    }
    

    col = pow(vec3(0.55,0.29,0.23), vec3(2.2));
    col = mix(col, vec3(1.,.1,.0) * .05 * 6., .6);
    col.r -= .05;
    col.g += .006;
    col.b += .006;
    col = mix(col, mix(col * 1.75, vec3(.2), .04), fade);

    return d2;
}




float map(vec3 p) {
    
//return fSofa(p / 10.) * 10.;

    float d = 1e12;

    p.x -= .2;
    p.x = abs(p.x);
    
    d = min(d, fBox(p, vec3(.5)));

    p -= vec3(1, .5, -.25);
    pR(p.xy, -.2);
    pR(p.xz, .8);
    pR(p.yz, .4);

    float scl = .5;
    p /= scl;

    p.x = abs(p.x);
    p.x -= .4;
    pR(p.xy, -1.);

    GIZMO(p);

    d = smin(d, fBox(p, vec3(.5)) * scl, .5);

    return d;
}

#pragma glslify: export(map)
