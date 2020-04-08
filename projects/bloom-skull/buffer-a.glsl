precision highp float;

uniform vec2 iResolution;
uniform float iTime;

uniform sampler2D volumeData; // volume-generate.glsl filter: linear wrap: clamp
uniform vec2 volumeDataSize;

// uniform sampler2D iChannel2; // buffer-b.glsl filter: linear wrap: mirror

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

vec3 stepPosition;
float stepScale;
mat3 stepRotate;

mat3 basisMatrix(vec3 forward, vec3 up) {
    vec3 ww = normalize(forward);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(uu, vv, ww);
}

mat3 orientMatrix(vec3 up, vec3 forward) {
    up.z *= -1.;
    vec3 uu = normalize(up);
    vec3 ww = normalize(cross(forward,uu));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(ww, uu, vv);
}

#pragma glslify: inverse = require(glsl-inverse)
#pragma glslify: import('./quat.glsl')
#pragma glslify: import('./camera.glsl')
#pragma glslify: easeOutSine = require(glsl-easings/sine-out)
#pragma glslify: easeOutCirc = require(glsl-easings/circular-out)

float skullOffset;
float skullRadius;
mat3 skullRotate;
vec3 bloomPosition;
mat3 bloomRotate;
float delay;

const float PHI = 1.61803398875;

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float range(float vmin, float vmax, float value) {
  return (value - vmin) / (vmax - vmin);
}

float rangec(float a, float b, float t) {
    return clamp(range(a, b, t), 0., 1.);
}

float almostIdentity(float x) {
    return x*x*(2.0-x);
}

float almostIdentityInv(float x) {
    x = 1. - x;
    return 1. - almostIdentity(x);
}

vec2 cmul (vec2 a, vec2 b) {
  return vec2(
    a.x * b.x - a.y * b.y,
    a.y * b.x + a.x * b.y
  );
}

vec2 cdiv (vec2 a, vec2 b) {
  float e, f;
  float g = 1.0;
  float h = 1.0;

  if( abs(b.x) >= abs(b.y) ) {
    e = b.y / b.x;
    f = b.x + b.y * e;
    h = e;
  } else {
    e = b.x / b.y;
    f = b.x * e + b.y;
    g = e;
  }

  return (a * g + h * vec2(a.y, -a.x)) / f;
} 

float circleFlat(vec2 p, float o) {
    p.x -= o;
    vec2 a = vec2(-o, 0);
    vec2 b = vec2(o, 0);
    // Complex function from rreusser https://www.shadertoy.com/view/tlcGzf
    vec2 p2 = cdiv(cmul(p - a, b), cmul(p - b, a));
    float d = length(p2);
    d = ((1. - d) * o) / (-1. - d);
    d += o;
    return d;
}

float circleFlatRadius(float x, float o) {
    x = o - x;
    float left = abs(x);
    float right = (o * o) / left;
    float d = max(0., right - left) * sign(x);
    return d / 2.;
}

#define Smooth

//Tri-linear Texturing Function
vec3 t3(sampler2D tex, vec3 p, vec3 n)
{
    p -= .5;
   // p -= .5;
    //mat3 R = mat3(vec3(cos(T),sin(T),0),vec3(-sin(T),cos(T),0),vec3(0,0,-1));
    //p *= R/8.0;
    //n *= R;
    #ifdef Smooth
 	return  (texture2D(tex,p.xy).rgb*n.z*n.z
            +texture2D(tex,p.zy).rgb*n.x*n.x
            +texture2D(tex,p.xz).rgb*n.y*n.y);
    #else
    return (texture2D(tex,p.xy).rgb
           +texture2D(tex,p.zy).rgb
           +texture2D(tex,p.xz).rgb)/3.0;
    #endif
}


float time;


vec2 round(vec2 a) {
    return floor(a + .5);
}


bool lightingPass;

struct Model {
    float d;
    vec3 p;
    bool isBloom;
    vec2 uv;
    vec2 cell;
    float wedges;
    float slice;
    float len;
};

Model newModel() {
    return Model(1e12, vec3(0), false, vec2(0), vec2(0), 0., 0., 0.);
}

Model opU(Model a, Model b) {
    if (a.d < b.d) {
        return a;
    } else {
        return b;
    }
}

#pragma glslify: import('./bloom.glsl')
//#pragma glslify: sdSkull = require(../skull/skull.glsl)
#pragma glslify: mapTex = require(./volume-read.glsl)


float sdSkull(vec3 p) {
    #ifdef MIRROR
        p.x = -abs(p.x);
    #endif
    p += OFFSET / SCALE;
    float bound = fBox(p, 1./SCALE);
    if ( ! lightingPass && bound > .01) {
        return bound;
    }
    // bound = max(bound, vmax(abs(mod(p + .0125, .025) - .0125)) - .003);
    p *= SCALE;
    float d = mapTex(volumeData, p, volumeDataSize);
    // d = min(d, bound);
    return d;
}


float drawSkull(vec3 p) {
    float s = 2.5;
    float d;

    // pR(p.xz, .8);
    //pR(p.xz, -.8);
    // pR(p.yz, -.3);

    float bound = length(p - vec3(0,-.2,0)) - .8 * s;
    if ( ! lightingPass && bound > .001) {
        return bound;
    }
    d = sdSkull((p.xyz * vec3(1,-1,-1)) / s) * s;
    // if (d < .1) {
    //     vec3 e = vec3(.01,0,0);
    //     // vec3 nor = normalize(vec3(
    //     //     sdSkull(((p.xyz + e.xyy) * vec3(1,-1,-1)) / s) * s,
    //     //     sdSkull(((p.xyz + e.yxy) * vec3(1,-1,-1)) / s) * s,
    //     //     sdSkull(((p.xyz + e.yyx) * vec3(1,-1,-1)) / s) * s
    //     // ));
    //     vec3 tex = t3(iChannel2, p/1.5, normalize(p));
    //     float disp = tex.r;
    //     disp = disp * 3. - 1.3;
    //     disp = smoothstep(-.5, 5., disp) * 10.;
    //     disp *= .3;
    //     disp = abs(disp - .1) - .1;
    //     d += disp * .03;
    // }
    return d;

    d = length(p) - 1.;
    d = abs(d) - .1;
    p.x = abs(p.x);
    d = smax(d, -(length(p - normalize(vec3(.3,.1,.6))) - .3), .1);
    d = smax(d, -(length(p - normalize(vec3(0,-.2,.8))) - .1), .1);
    d = smax(d, -(length(p - normalize(vec3(0,-1,0))) - .7), .1);
    return d;
}

//#define DEBUG_BLOOMS

float drawSkullWithBlooms(vec3 p, float t) {
    float scale = skullRadius;
    p /= scale;
    float d = drawSkull(p);

    #ifdef DEBUG_BLOOMS
        if (t > .8) { // when is it realistic to start showing blooms
            d = min(d, length(p.xz) - .1);
        }
    #endif

    return d * scale;
}

void applyMat4(inout vec3 p, mat4 m) {
    p = (vec4(p, 1) * m).xyz;
}

vec3 opU(vec3 a, vec3 b) {
    return a.x < b.x ? a : b;
}

void tweenSkull(inout vec3 p, inout float scale, float t) {
    float skullHeight = mix(.2, skullOffset, easeOutSine(rangec(.55, 1.5, t)));
    float skullScale = mix(.0, 1., easeOutSine(rangec(.45, 1., t)));
    p.y -= skullHeight;
    p *= skullRotate;

    float rt = 1. - easeOutCirc(rangec(.0, 3., t));
    pR(p.yz, rt * 5.);

    p /= skullScale;
    scale *= skullScale;
}

Model drawFinalBloom(vec3 p, float t, float scale) {

    float density = 2.;
    float thickness = .05;
    float pointy = 1.;
    float width = .4;

    float bt = smoothstep(0., 2., t);
    bt = easeOutCirc(bt);
    float bs = 1.4;
    Model blm = drawBloom(p / bs, bt, density, thickness, pointy, width, true);
    blm.d *= bs * scale;
    return blm;
}

Model drawBloom(vec3 p, float t, float scale, float density, float thickness, float pointy, float width) {
    p /= scale;
    Model model = drawBloom(p, t, density, thickness, pointy, width, false);
    model.d *= scale;
    return model;
}


Model skullWithBloom(inout vec3 p, inout float scale, inout float t) {
    
    if (t <= .0 || scale <= 0.) {
        return newModel();
    }

    // skull with sub blooms
    float d = drawSkullWithBlooms(p, t);
    Model model = newModel();
    model.d = d;

    Model bloom;
    vec3 pp = p;

    float density = 2.;
    float thickness = .05;
    float pointy = 1.;
    float width = .4;

    p -= vec3(-.2,.2,.25)*1.05;
    p *= orientMatrix(vec3(-1,.7,-.9), vec3(0,1,0));
    density = 1.;
    thickness = .05;
    pointy = 0.;
    width = .4;
    bloom = drawBloom(p, t-.5, .08, density, thickness, pointy, width);
    model = opU(model, bloom);
    p = pp;

    p -= vec3(.28,.1,.15);
    p *= orientMatrix(vec3(1,-.1,-.2), vec3(1,1,0));
    density = 2.5;
    thickness = .1;
    pointy = 0.;
    width = .2;
    bloom = drawBloom(p, t-.8, .1, density, thickness, pointy, width);
    model = opU(model, bloom);
    p = pp;

    p -= vec3(.22,.23,.2) * 1.05;
    p *= orientMatrix(vec3(.5,.3,-.2), vec3(1,1,0));
    density = .5;
    thickness = .1;
    pointy = 0.;
    width = .5;
    bloom = drawBloom(p, smoothstep(1.3, 1.8, t), .05, density, thickness, pointy, width);
    model = opU(model, bloom);
    p = pp;


    model.d *= scale;

    // set location for next bloomWithSkull
    // this is the camera
    p -= bloomPosition;
    p /= stepScale;
    p *= bloomRotate;
    scale *= stepScale;
    t -= delay;

    bloom = drawFinalBloom(p, t, scale);
    model = opU(model, bloom);

    return model;
}

// C:\Program Files (x86)\Google\Chrome\Application\chrome.exe --use-angle=gl
Model map(vec3 p) {

    float scale = 1.;
    float t = time;

    #ifdef DEBUG_BLOOMS
        t += .0001;
        t *= delay;
        scale = 3.;
        p /= scale;
        pR(p.yz, -1.9);
        pR(p.xy, -2.3);
        pR(p.xz, -.4 + t/2.);
        return skullWithBloom(p, scale, t);
    #endif

    t += 1.;
    float camScale = tweenCamera(p, t);

    float w = mapCameraDebug(p);

    Model model = newModel();
    Model model2;

    t *= delay;
    t += 1.;

    // 4 iterations
    // t += delay;
    // scale /= stepScale;
    // p *= inverse(bloomRotate);
    // p *= stepScale;
    // p += bloomPosition;

    // 3 iterations
    model = drawFinalBloom(p, t, scale);

    for (float i = 0.; i < 4.; i++) {
        tweenSkull(p, scale, t);
        model2 = skullWithBloom(p, scale, t);
        model = opU(model, model2);
    }

    model.d /= camScale;

    return model;
}

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

// normal function, call de() in a for loop for faster compile times.
// vec3 calcNormal(vec3 p) {
//     vec4 n = vec4(0);
//     for (int i = 0 ; i < 4 ; i++) {
//         vec4 s = vec4(p, 0);
//         s[i] += 0.001;
//         n[i] = map(s.xyz).x;
//     }
//     return normalize(n.xyz-n.w);
// }

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

// #define AA 3

// https://www.shadertoy.com/view/lsKcDD
float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    float res = 1.0;
    float t = mint;
    float ph = 1e10;
    
    for( int i=0; i<64; i++ )
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
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).d;
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}

vec3 doShading(vec3 pos, vec3 rd, Model model) {
    vec3 col = vec3(.3);

    if (model.isBloom) {
        col = vec3(.3,.05,.05);
        col = vec3(.5,1,.8) * .1;
        //col += vec3(.06,.0,.03) * max(1. - 1. / 2., 0.);
        //col = mix(col, col * .2, 0.);
    }

            lightingPass = true;

			vec3 nor = calcNormal(pos);
            float occ = calcAO( pos, nor );
            vec3  lig = normalize( vec3(-.2, 1.5, .3) );
            vec3  lba = normalize( vec3(.5, -1., -.5) );
            vec3  hal = normalize( lig - rd );
            float amb = sqrt(clamp( 0.5+0.5*nor.y, 0.0, 1.0 ));
            float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
            float bac = clamp( dot( nor, lba ), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
            float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );

            occ = mix(1., occ, .8);
            
            dif *= softshadow( pos, lig, 0.001, .9 );

            float spe = pow( clamp( dot( nor, hal ), 0.0, 1.0 ),16.0)*
                        dif *
                        (0.04 + 0.96*pow( clamp(1.0+dot(hal,rd),0.0,1.0), 5.0 ));

            vec3 lin = vec3(0.0);
            
            // lin += 2.80*dif*vec3(1.30,1.00,0.70);
            // lin += 0.55*amb*vec3(0.40,0.60,1.15)*occ;
            // lin += 1.55*bac*vec3(0.25,0.25,0.25)*occ*vec3(2,0,1);
            // lin += 0.25*fre*vec3(1.00,1.00,1.00)*occ;
        	// col = col*lin;
			// col += 5.00*spe*vec3(1.10,0.90,0.70);

        lin += 3.80*dif*vec3(1.30,1.00,0.70);
        lin += 0.55*amb*vec3(0.40,0.60,1.15)*occ;
        lin += 0.55*bac*vec3(0.25,0.25,0.25)*occ;
        lin += 0.25*fre*vec3(1.00,1.00,1.00)*occ;
		col = col*lin;
		col += 7.00*spe*vec3(1.10,0.90,0.70);


    //float lig = clamp(dot(nor, vec3(0,1,0)), .01, 1.);
    //col *= lig;

    return col;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    skullOffset = 1.8;
    skullRadius = .3;
    skullRotate = basisMatrix(vec3(.5,1,-.4), vec3(-.9,1.,0));
    skullRotate = basisMatrix(vec3(.9,1,.9), vec3(1,0,1));

    bloomPosition = vec3(.8,.8,-.1) * skullRadius * 1.1;
    // bloomPosition += vec3(0,0,.1);
    bloomRotate = basisMatrix(vec3(-.5,1,0), vec3(1,0,0));
    //  bloomPosition -= vec3(0,0,.1);

    stepPosition = vec3(0,skullOffset,0) + skullRotate * bloomPosition;
    stepScale = .15;
    stepRotate = skullRotate * bloomRotate;

    // position 2  =  position  +  rotation * position

    delay = 1.5;

    cameraPrecalc();
    calcPhyllotaxis();

    vec3 col;
    vec3 tot = vec3(0.0);

    float mTime =iTime/6.;

    #ifndef DEBUG_BLOOMS
        mTime = mod(mTime, 1.);
    #endif

    time = mTime;
    // time = 1.;

    vec2 o = vec2(0);
    float depth;

    #ifdef AA
    for( int m=0; m<AA; m++ )
    for( int n=0; n<AA; n++ )
    {
    // pixel coordinates
    o = vec2(float(m),float(n)) / float(AA) - 0.5;
    // time coordinate (motion blurred, shutter=0.5)
    float d = 0.5*sin(fragCoord.x*147.0)*sin(fragCoord.y*131.0);
    time = mTime - 0.1*(1.0/24.0)*(float(m*AA+n)+d)/float(AA*AA-1);
    #endif

        vec2 p = (-iResolution.xy + 2.0*(fragCoord+o))/iResolution.y;

        //fragColor = texture2D(iChannel2, fragCoord.xy/iResolution.xy); return;

        vec3 camPos = eye;
        vec3 rayDirection = normalize(dir);

        // mat3 camMat = calcLookAtMatrix( camPos, vec3(0,.23,-.35), -1.68);
        // rayDirection = normalize( camMat * vec3(p.xy,2.8) );

        vec3 rayPosition = camPos;
        float rayLength = 0.;
        float dist = 0.;
        bool bg = false;
        lightingPass = false;
        Model model;
        const float MAX_DIST = 100.;

        for (int i = 0; i < 300; i++) {
            rayLength += dist;
            rayPosition = camPos + rayDirection * rayLength;
            model = map(rayPosition);
            dist = model.d;

            if (abs(dist) < .00001) {
                break;
            }
            
            if (rayLength > MAX_DIST) {
                bg = true;
                break;
            }
        }

        vec3 bgCol = vec3(.02,.0,.0);
        bgCol = vec3(.005,0,.007);
        col = bgCol;

        
        if ( ! bg) {
            col = doShading(rayPosition, rayDirection, model);
            float fog = 1. - exp((rayLength - 3.) * -.5);
            col = mix(col, bgCol, clamp(fog, 0., 1.)); 
        }

        tot += col;
        depth += rayLength / MAX_DIST;
    #ifdef AA
    }
    tot /= float(AA*AA);
    depth /= float(AA*AA);
    #endif

    col = tot;
    fragColor = vec4(col, depth);
}
