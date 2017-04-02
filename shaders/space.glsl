
#pragma glslify: starField = require(./starfield.glsl)

// --------------------------------------------------------
// Gamma
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

const float GAMMA = 2.2;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}

vec3 screenToLinear(vec3 screenRGB) {
    return gamma(screenRGB, GAMMA);
}

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}


// https://www.shadertoy.com/view/lslGWr
// http://www.fractalforums.com/new-theories-and-research/very-simple-formula-for-fractal-patterns/

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 nebulaPal(float t) {
	return pal(t, vec3(1.,.9,1.),vec3(0.4,0.3,0.),vec3(1.5,1.5,0.),vec3(.2,0.05,0.0));    
}

void nebulaCalc(int i, float strength, inout vec3 p, vec3 seed, inout float r, inout float prev) {
    float mag = dot(p, p);
    p = abs(p) / mag + seed;
    float w = exp(-float(i) / 7.);
    r = w * exp(-strength * pow(abs(mag - prev), 2.3));
    prev = mag;
}
        
vec3 nebulaField(vec2 uv) {
    uv *= .25;
    uv += vec2(5.2, -6.2);
    pR(uv, .38);
    
    vec3 seed = vec3(-.34, -.55, -1.579);     
    vec3 offset = vec3(50., 50., 90.); 	
    vec3 p = (vec3(uv, 0.) / .1) + offset;
	float strength = 15.;
    
    float r, accum = 0., prev = 0., tw = 0.;
    vec3 col = vec3(0);
	for (int i = 0; i < 32; ++i) {
        nebulaCalc(i, strength, p, seed, r, prev);
		accum += r;
        if (i > 10) {
			col -= r * vec3(.8,1.,.9) * 2.;
        } else if (i > 9) {
            float t = sin(accum * 3.142 - 1.8) * .5 + .5;
            col = nebulaPal(t);
        }
	}
    return col;
}

vec3 nebulaSeam(vec2 uv) {
    vec3 seed = vec3(-.34, -.55, -1.579);     
    
    vec3 offset = vec3(170. + 10., -70. + 100., 40. + 120.);
    
    uv *= .7;
    vec3 p = vec3(uv / .1, 0.) + offset;
	float strength = 20.;

    
    float r, accum = 0., prev = 0., tw = 0.;
    vec3 col = vec3(0);
	for (int i = 0; i < 32; ++i) {
        nebulaCalc(i, strength, p, seed, r, prev);
        if (i < 18) {
            accum += r * .9;
        } else {
            accum -= r * .7;
        }
	}
    accum -= .05;
    accum = pow(accum, 10.);
    accum = clamp(accum, 0., 1.);
    col = vec3(accum);
 	           
    return col;
}


vec3 space(vec2 uv) {
    
    vec3 soffset = vec3(7.9,3.001,0.15);
    vec3 stars = starField(uv, soffset) * .01;
    
    vec3 field = nebulaField(uv);

    vec3 seam = nebulaSeam(uv);
    
    vec3 col = field + stars;
    //col -= seam;
    
    col *= .9;
    col += .2;
    
    col = clamp(col, 0., 1.);
    col = screenToLinear(col);
    
    return col;
}


#pragma glslify: export(space)
