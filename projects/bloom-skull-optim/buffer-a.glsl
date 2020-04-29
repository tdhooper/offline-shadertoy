// framebuffer tile: 5

precision highp float;

#extension GL_OES_standard_derivatives : enable

uniform vec2 iResolution;

uniform sampler2D volumeData; // volume-generate.glsl filter: linear wrap: clamp
uniform vec2 volumeDataSize;
uniform mat4 cameraMatrix;

uniform float guiDensityStart;
uniform float guiDensityEnd;
uniform float guiThickness;
uniform float guiPointy;
uniform float guiWidth;
uniform float guiSize;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

/* SHADERTOY FROM HERE */

#define ZERO 0
#define PI 3.1415926
#define HALF_PI 1.5707963267948966

mat3 basisMatrix(vec3 forward, vec3 up) {
    vec3 ww = normalize(forward);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(uu, vv, ww);
}

mat3 orientMatrix(vec3 up, vec3 forward) {
    mat3 m = basisMatrix(up, forward);
    return mat3(m[0], m[2], -m[1]);
}



//========================================================
// Fractal camera loop
// https://www.shadertoy.com/view/wslyzH
//========================================================

vec3 stepPosition;
float stepScale;
mat3 stepRotate;

#pragma glslify: inverse = require(glsl-inverse)
#pragma glslify: import('./quat.glsl')
#pragma glslify: import('./camera.glsl')

//========================================================
// Modelling
//========================================================

// Utils
// HG_SDF, stack.gl
//--------------------------------------------------------

float easeOutSine(float t) {
  return sin(t * HALF_PI);
}

float easeOutCirc(float t) {
  return sqrt((2.0 - t) * t);
}

const float PHI = 1.61803398875;

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec2 pR2d(vec2 p, float a) {
    return cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float cmax(float a, float b, float r) {
    return max(max(a, b), (a + r + b)*sqrt(0.5));
}

float range(float vmin, float vmax, float value) {
  return (value - vmin) / (vmax - vmin);
}

float rangec(float a, float b, float t) {
    return clamp(range(a, b, t), 0., 1.);
}

mat3 rotX(float a) {
	return mat3(
    	1, 0, 0,
        0, cos(a), -sin(a),
        0, sin(a), cos(a)
    );
}

mat3 rotY(float a) {
	return mat3(
    	cos(a), 0, sin(a),
        0, 1, 0,
        -sin(a), 0, cos(a)
    );
}

mat3 rotZ(float a) {
	return mat3(
    	cos(a), -sin(a), 0,
        sin(a), cos(a), 0,
        0, 0, 1
    );
}

vec2 round(vec2 a) {
    return floor(a + .5);
}


// Types
//--------------------------------------------------------

struct Model {
    float d;
    vec3 p;
    bool isBound;
    int id;
    bool isBloom;
    vec2 uv;
    vec2 cell;
    float wedges;
    float slice;
    float len;
    float neg; // subtract from distance
    float crackdepth;
};

Model newModel() {
    return Model(1e12, vec3(0), false, 0, false, vec2(0), vec2(0), 0., 0., 0., 1e12, 0.);
}

Model opU(Model a, Model b) {
    Model m = a;
    if (b.d < a.d) {
        m = b;
    }
    m.neg = min(a.neg, b.neg);
    return m;
}


// Config
//--------------------------------------------------------

float skullOffset;
float skullRadius;
mat3 skullRotate;
vec3 bloomPosition;
mat3 bloomRotate;
float delay;

float time;
bool lightingPass;

float boundEps;
float globalScale;

const float CUTOFF = 3.6; // remove old itrerations when they're out of view

struct BloomSpec {
    vec2 density;
    float thickness;
    float width;
    float pointy;
    float size;
    bool hideInside;
};

struct BloomInstance {
	vec3 pos;
    mat3 rot;
    float offset;
    float start;
    float stop;
    BloomSpec spec;
};

BloomInstance bloomInstances[4];

void precalcBloomInstances() {

    bloomInstances[0] = BloomInstance(
        vec3(-.2,.2,.25), //pos
        orientMatrix(vec3(-1,.7,.9), vec3(0,1,0)), //rot
        .02, //offset
        -.8, //start
        1., //stop
        BloomSpec(
            vec2(.08, 1.), //density
            .11, //thickness
            .39, //width
            .4, //pointy
            .11, //size
            false //hideInside
        )
    );
    
	bloomInstances[1] = BloomInstance(
        vec3(-.135,.105,.35) * .98, //pos
        orientMatrix(vec3(-.7,.2,.9), vec3(0,1,0)), //rot
        .0, //offset
        -1.1, //start
        .8, //stop
        BloomSpec(
            vec2(.48, 1.8), //density
            .08, //thickness
            .45, //width
            .0, //pointy
            .07, //size
            false //hideInside
        )
    );
    
	bloomInstances[2] = BloomInstance(
        vec3(.22,.23,.2), //pos
        orientMatrix(vec3(.5,.3,.2), vec3(1,1,0)) * rotY(.3), //rot
        .05, //offset
        -.1, //start
        1.5, //stop
        BloomSpec(
            vec2(.12, 1.45), //density
            .07, //thickness
            .17, //width
            .0, //pointy
            .14, //size
            false //hideInside
        )
    );
    
	bloomInstances[3] = BloomInstance(
        vec3(.28,.07,.18) * .85, //pos
        orientMatrix(vec3(1,-.3,.5), vec3(1,1,0)), //rot
        .1176, //offset
        -.5, //start
        1.5, //stop
        BloomSpec(
            vec2(.15, 1.45), //density
            .15, //thickness
            .41, //width
            .0, //pointy
            .085, //size
            false //hideInside
        )
    );   
}


struct CrackInstance {
    vec3 pos;
    mat3 rot;
    float start;
    float stop;
    vec2 weight;

	float angle;
    vec2 offset;
    vec2 size;
    float lines;
    float seed;
};

CrackInstance crackInstances[7];

void precalcCrackInstances() {

    crackInstances[0] = CrackInstance(
    	vec3(.28,.1,.15), //pos
    	orientMatrix(vec3(1,-.1,.2), vec3(1,1,0)) * rotY(2.2) * rotZ(.9), //rot
    	-.3, //start
    	.7, //stop
    	vec2(.001, .03), //weight
		0., //angle
    	vec2(.015,-.02), //offset
    	vec2(.15,.03), //size
    	18., //lines
    	11. //seed
    );

    crackInstances[1] = CrackInstance(
    	vec3(.28,.18,.18)*.98, //pos
    	orientMatrix(vec3(1,.4,.4), vec3(1,1,0)), //rot
    	-.7, //start
    	.3, //stop
    	vec2(.001, .03), //weight
		1., //angle
    	vec2(0), //offset
    	vec2(.12,.03), //size
    	13., //lines
    	14. //seed
    );
    
    crackInstances[2] = CrackInstance(
    	vec3(.28,.18,.18)*.98, //pos
    	orientMatrix(vec3(1,.4,.4), vec3(1,1,0)), //rot
    	-.7, //start
    	.3, //stop
    	vec2(.001, .03), //weight
		3.6, //angle
    	vec2(0), //offset
    	vec2(.12,.03), //size
    	16., //lines
    	16. //seed
    );
    
    crackInstances[3] = CrackInstance(
        bloomInstances[0].pos - bloomInstances[0].rot * vec3(0,0,.02), //pos
        bloomInstances[0].rot, //rot
    	-1., //start
    	.0, //stop
    	vec2(.001, .03)*1.2, //weight
		1., //angle
    	vec2(.015,-.02)*1.2, //offset
    	vec2(.12,.05)*1.2, //size
    	10., //lines
    	1. //seed
    );
    
    crackInstances[4] = CrackInstance(
        bloomInstances[0].pos - bloomInstances[0].rot * vec3(0,0,.02), //pos
        bloomInstances[0].rot, //rot
    	-1., //start
    	.0, //stop
    	vec2(.001, .03)*1.2, //weight
		3., //angle
    	vec2(0), //offset
    	vec2(.12,.05)*1.2, //size
    	10., //lines
    	4. //seed
    );
    
    crackInstances[5] = CrackInstance(
        bloomInstances[0].pos - bloomInstances[0].rot * vec3(0,0,.02), //pos
        bloomInstances[0].rot, //rot
    	-1., //start
    	.0, //stop
    	vec2(.001, .03)*1.2, //weight
		5.5, //angle
    	vec2(-.01,.02)*1.2, //offset
    	vec2(.08,.02)*1.2, //size
    	12., //lines
    	3. //seed
    );
    

    crackInstances[6] = CrackInstance(
        vec3(.3,.29,-.03), //pos
        orientMatrix(vec3(.5,.3,.05), vec3(1,0,1)), //rot
    	-.7, //start
    	.6, //stop
    	vec2(.001, .1), //weight
		3.2, //angle
    	vec2(0), //offset
    	vec2(.18,.06), //size
    	15., //lines
    	17. //seed
    );
}


// Bloom model
// https://www.shadertoy.com/view/WtGXWm
//--------------------------------------------------------

#pragma glslify: import('./bloom.glsl')

Model drawBloom(vec3 p, float t, BloomSpec spec) {
    p /= spec.size;
    globalScale *= spec.size;
    Model model;
    float bound = length(p) - mix(.7, mix(.8, 2.4, spec.width), t);
    if ( ! lightingPass && bound > boundEps / globalScale) {
		model = newModel();
        model.d = bound;
        model.neg = bound;
        model.isBound = true;
    } else {
        model = drawBloom(
            p,
            t,
            spec.density,
            spec.thickness,
            spec.pointy,
            spec.width,
            spec.hideInside
        );
    }
    model.d *= spec.size;
    model.neg *= spec.size;
    globalScale /= spec.size;
	return model;
}


// Crack model
//--------------------------------------------------------

#pragma glslify: fCrack = require(./crack.glsl)



// Skull model
//--------------------------------------------------------

#pragma glslify: mapTex = require(./volume-read.glsl)

Model sdSkull(vec3 p) {
    Model model = newModel();
    float rad = .3;
    float bound = fBox(p - vec3(0,-.13,-.02), vec3(.45,.43,.54) - rad) - rad;
    bound = smin(bound, fBox(p - vec3(0,.29,-.39), vec3(.25,.2,.18)), .2);
    bound = max(bound, dot(p.zy - vec2(0,.36), normalize(vec2(.27,1))));
    if ( ! lightingPass && bound > boundEps / globalScale) {
        model.d = bound;
        model.isBound = true;
        return model;
    }

    p.x = -abs(p.x);
    p += OFFSET / SCALE;
    p *= SCALE;
    model.d = mapTex(volumeData, p, volumeDataSize);

    if (lightingPass && bound > .01) {
        model.d += bound;
        model.isBound = true;
        return model;
    }

    return model;
}

Model drawSkull(vec3 p, float t) {
    float scale = 2.5 * skullRadius;
    p /= scale;
    globalScale *= scale;
    Model model = sdSkull((p.xyz * vec3(1,-1,-1)));
    model.d *= scale;
    globalScale /= scale;
    return model;
}


// Composition
//--------------------------------------------------------

Model drawFinalBloom(vec3 p, float t) {
    float bt = smoothstep(0., 2., t);
    bt = easeOutCirc(bt);
    pR(p.xz, .7 * PI * 2.);
    Model bloom = drawBloom(
        p,
        bt,
        BloomSpec(
        	vec2(.15, 2), //density
            .05, //thickness
            .4, //width
            1., //pointy
            1.4, //size
            true //hideInside
        )
   	);
    bloom.id = 5;
    return bloom;
}

float fTri(vec2 p, float radius) {
    radius /= 2.;
    vec2 a = normalize(vec2(1.6,1.));
    return max(
        dot(p, vec2(0,-1)) - radius,
        max(
        	dot(p, a) - radius,
        	dot(p, a * vec2(-1,1)) - radius
        )
    );
}

Model skullWithBlooms(vec3 p, float t) {
    
    if (t <= .0 || globalScale <= 0.) {
        return newModel();
    }
    
    Model model = newModel();    
    Model bloom;

    if (t < CUTOFF) {
        // skull with sub blooms
        Model skull = drawSkull(p, t);
        float skulld = skull.d;
        skull.crackdepth = max(-skull.d, 0.);
        float td = t - delay;
        model = skull;
        //return skull;
        
        vec3 pp = p;
        
        // cracks
        if (skull.d < boundEps / globalScale) {
	        CrackInstance ci;
	        float cracks = 1e12;
            float crack;
            for (int i = ZERO; i < 7; i++) {
                ci = crackInstances[i];
                p -= ci.pos;
                p *= ci.rot;
                if (i == 6) {
                	crack = min(crack, max(fTri(p.xz, .45), fTri(p.xz * vec2(1,-1), .3)));
                } else {
                    crack = 1e12;
                }
                pR(p.xz, ci.angle);
                float blend = smoothstep(ci.start, ci.stop, td);
                float weight = mix(ci.weight.x, ci.weight.y, blend);
                float crack = min(crack, fCrack(p.xz - ci.offset, ci.size, ci.lines, ci.seed, weight));
                crack += (1.-blend) * weight/2.;
                crack -= min(skull.d * mix(1.5, .2, blend), 0.);
                crack = max(crack, -(p.y + .25)); // stop it poking through the other side
                cracks = min(crack, cracks);
                p = pp;
            }
            cracks = max(cracks, -(skulld + .06)); // limit depth
            skull.d = cmax(skull.d, -cracks, .003);
      	}
        
        // blooms
	    Model blooms = newModel();
        BloomInstance bi;
        for (int i = ZERO; i < 4; i++) {
            bi = bloomInstances[i];
            float bt = smoothstep(bi.start, bi.stop, td);
            p -= bi.pos * (1. + bi.offset * bt);
            p *= bi.rot;
            bloom = drawBloom(p, bt, bi.spec);
            bloom.id = i + 1;
            skull.d = max(skull.d, -bloom.neg);
            blooms = opU(blooms, bloom);
            p = pp;
        }

        model = opU(skull, blooms);
    }

    return model;
}

void tweenSkull(inout vec3 p, float t) {
    float skullHeight = mix(.2, skullOffset, easeOutSine(rangec(.55, 1.5, t)));
    float skullScale = mix(.0, 1., easeOutSine(rangec(.45, 1., t)));
    p.y -= skullHeight;
    p *= skullRotate;

    float rt = 1. - easeOutCirc(rangec(.0, 3., t));
    pR(p.yz, rt * 5.);

    p /= skullScale;
    globalScale *= skullScale;
}

void stepTransform(inout vec3 p, inout float t) {
    p -= bloomPosition;
    p /= stepScale;
    globalScale *= stepScale;
    p *= bloomRotate;
    t -= delay;
}

Model map(vec3 p) {

    globalScale = 1.;
    float t = time;

    float off = .73;
    
    t += 1.;
    float camScale = tweenCamera(p, t - (1.-off)/delay);
    globalScale *= camScale;

    Model model = newModel();
    Model model2;

    t *= delay;
    t += off;

    if (t < 2.6) {
    	model = drawFinalBloom(p, t);
      	model.d *= globalScale;
    }

    for (float i = 0.; i < 3.; i++) {

        // scale
        tweenSkull(p, t);

        if (globalScale <= 0.) {
			break;
        }

        // skull
        model2 = skullWithBlooms(p, t);
        model2.d *= globalScale;
        model = opU(model, model2);

        // translate
        stepTransform(p, t);
        
        // bloom
        model2 = drawFinalBloom(p, t);
        model2.d *= globalScale;
        model2.neg *= globalScale;
        if ( ! model2.isBound) {
            model.d = smax(model.d, -model2.neg, .04 * globalScale);
        }
        model = opU(model, model2);
    }

    return model;
}


//========================================================
// Rendering
//========================================================

// Shading
//--------------------------------------------------------

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    vec3 npos;
    for (int i = 0; i < NORMAL_STEPS; i++){
        npos = pos + eps * invert;
        nor += map(npos).d * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

// https://www.shadertoy.com/view/lsKcDD
float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    #ifdef DISABLE_SHADOWS
        return 1.;
    #endif
    float res = 1.0;
    float t = mint;
    float ph = 1e10;
    
    for( int i=ZERO; i<512; i++ )
    {
        float h = map( ro + rd*t ).d;
        res = min( res, 10.0*h/t );
        t += h;
        if( res<0.0001 || t>tmax ) break;
        
    }
    return clamp( res, 0.0, 1.0 );
}

// https://www.shadertoy.com/view/Xds3zN
float calcAO( in vec3 pos, in vec3 nor )
{
    #ifdef DISABLE_SHADOWS
        return 1.;
    #endif
    float occ = 0.0;
    float sca = 1.0;
    for( int i=ZERO; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).d;
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}

vec3 worldToCam(vec3 v) {
    return (vec4(v, 1) * cameraMatrix).xyz;
}

vec3 doShading(vec3 pos, vec3 rd, Model model) {
    vec3 col = vec3(.3);
    float cracksha = rangec(.001, .0, model.crackdepth);
	float crackocc = rangec(.05, .0, model.crackdepth);

    if (model.isBloom) {
		crackocc = 1.;
        cracksha = 1.;
        col = vec3(.04,.09,.09);
        col = mix(vec3(.07,.025,.06), col, smoothstep(-.2, .0, model.wedges) * model.uv.y);
        if (model.id == 1) {
            col = vec3(.1,.06,.1);
            col = mix(col, vec3(.095,.02,.055), smoothstep(-.1, .0, model.wedges) * smoothstep(.5, 1., model.uv.y));
        }
        if (model.id == 2) {
            col = vec3(.04,.07,.09);
            col = mix(col, vec3(.005,.015,.02), smoothstep(.5, 1.3, model.cell.y));
        }
        if (model.id == 3) {
            col = vec3(.04,.07,.09);
            col = mix(col, vec3(.2), smoothstep(.4, 1., model.uv.y));
        }
        if (model.id == 4) {
            col = vec3(.11,.05,.1);
            col = mix(col, vec3(.5), smoothstep(.5, 2., model.cell.y));
        }
    }

    vec3 nor = calcNormal(pos);
    lightingPass = true;

    // IQ's shading, I always find this so nice to work with
    // https://www.shadertoy.com/view/Xds3zN
    float occ = calcAO( pos, nor );
    vec3  lig = normalize( worldToCam(vec3(.5, 1, .2)) );
    vec3  lba = normalize( worldToCam(vec3(-.5, -.8, .1)) );
    vec3  hal = normalize( lig - rd );
    float amb = sqrt(clamp( 0.5+0.5*worldToCam(nor).y, 0.0, 1.0 ));
    float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
    float bac = clamp( dot( nor, lba ), 0.0, 1.0 )*clamp( 1.0-worldToCam(nor).y,0.0,1.0);
    float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );

    occ = mix(1., occ, .8) * mix(1., crackocc, .9);

    float sha = softshadow( pos, lig, 0.001, .9 ) * cracksha;
    dif *= sha;

    float spe = pow( clamp( dot( nor, hal ), 0.0, 1.0 ),16.0)*
        dif *
        (0.04 + 0.96*pow( clamp(1.0+dot(hal,rd),0.0,1.0), 5.0 ));

    vec3 lin = vec3(0.0);
    lin += 3.80*dif*vec3(1.30,1.00,0.70);
    lin += 0.55*amb*vec3(0.40,0.60,1.15)*occ;
    lin += 0.55*bac*vec3(0.4,0.25,0.3)*occ;
    lin += 0.15*fre*vec3(1.00,1.00,1.00)*occ;
    col = col*lin;
    col += 7.00*spe*vec3(1.10,0.90,0.70);

    return col;
}


// Debug
//--------------------------------------------------------

// https://www.shadertoy.com/view/ll2GD3
vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}
vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


// Main
//--------------------------------------------------------

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    // Config

    skullOffset = 1.8;
    skullRadius = .3;
    skullRotate = basisMatrix(vec3(.9,1,.9), vec3(1,0,1));

    bloomPosition = vec3(.8,.8,-.1) * skullRadius * 1.1;
    bloomRotate = basisMatrix(vec3(-.3,1,0), vec3(1,0,0));

    stepPosition = vec3(0,skullOffset,0) + skullRotate * bloomPosition;
    stepScale = .15;
    stepRotate = skullRotate * bloomRotate;

    delay = 1.5;

    cameraPrecalc();
    calcPhyllotaxis();
    precalcBloomInstances();
    precalcCrackInstances();

    time = loopTime(iTime);

    vec3 col;
    float depth;
    
    // Raymarch (enhanced sphere tracing)
    // https://erleuchtet.org/~cupe/permanent/enhanced_sphere_tracing.pdf

    vec3 camPos = eye;
    vec3 rayPosition = camPos;
    vec3 rayDirection = normalize(dir);
    float rayLength = 0.;
    float stepLength = 0.;

    bool bg = false;
    lightingPass = false;

    Model model;
    Model candidateModel;
    float error;
    float candidateError = 1e12;
    float canidateRayLength;
    
    const float MAX_DIST = 16.;
    const int MAX_STEPS = 600;
    float debugSteps = 0.;
    
    float pixelRadius = fwidth((camPos + rayDirection).x);
	float overstep = 1.;
    float radius;
    float signedRadius;
    float previousRadius;
    
    for (int i = 0; i < MAX_STEPS; i++) {
        debugSteps = float(i);
        
        rayPosition = camPos + rayDirection * rayLength;
        boundEps = pixelRadius * rayLength * 2.;
        model = map(rayPosition);
        
        signedRadius = model.d;
        radius = abs(signedRadius);

        bool overshot = false;
        stepLength = signedRadius * .7;
        // bool overshot = overstep > 1. && ! model.isBound && (radius + previousRadius) < stepLength;
        // if (overshot) {
        //     stepLength -= overstep * stepLength;
        //     overstep = 1.;
        // } else {
        //     stepLength = signedRadius * overstep;
        // }

        previousRadius = radius;

        float error = radius / rayLength;
        if ( ! model.isBound && ! overshot && error < candidateError) {
            candidateModel = model;
            candidateError = error;
            canidateRayLength = rayLength;
        }

        if ( ! model.isBound && ! overshot && error < pixelRadius) {
            break;
        }
        
    	if (rayLength > MAX_DIST) {
            bg = true;
            canidateRayLength = rayLength;
            break;
        }

        rayLength += stepLength;
    }
    
    model = candidateModel;
    rayLength = canidateRayLength;

    vec3 bgCol = vec3(.007,0,.007);
    col = bgCol;

    if ( ! bg) {
        col = doShading(rayPosition, rayDirection, model);
        float fog = 1. - exp((rayLength - 3.) * -.5);
        col = mix(col, bgCol, clamp(fog, 0., 1.)); 
    }
    
    depth = rayLength / MAX_DIST;
    depth = smin(depth, .8, .1); // stop the DOF 'pop' when background objects are removed

    //col = spectrum(debugSteps / float(MAX_STEPS));
	//col = spectrum(depth);

    fragColor = vec4(col, depth);
}
