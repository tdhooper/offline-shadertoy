precision highp float;

uniform vec2 iResolution;
uniform float iTime;

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


float time;


vec2 round(vec2 a) {
    return floor(a + .5);
}



struct Model {
    float d;
    vec3 p;
    vec2 uv;
    vec2 cell;
    float wedges;
    float slice;
    float len;
};

Model opU(Model a, Model b) {
    if (a.d < b.d) {
        return a;
    } else {
        return b;
    }
}

#pragma glslify: import('./bloom.glsl')
#pragma glslify: sdSkull = require(../skull/skull.glsl)

float drawSkull(vec3 p) {
    float s = 2.5;
    // pR(p.xz, .8);
    //pR(p.xz, -.8);
    // pR(p.yz, -.3);
    float bound = length(p - vec3(0,-.2,0)) - .8 * s;
    if (bound > .001) {
        return bound;
    }
    return sdSkull((p.xyz * vec3(1,-1,-1)) / s) * s;

    float d = length(p) - 1.;
    d = abs(d) - .1;
    p.x = abs(p.x);
    d = smax(d, -(length(p - normalize(vec3(.3,.1,.6))) - .3), .1);
    d = smax(d, -(length(p - normalize(vec3(0,-.2,.8))) - .1), .1);
    d = smax(d, -(length(p - normalize(vec3(0,-1,0))) - .7), .1);
    return d;
}

float drawSkullWithBlooms(vec3 p, float t) {
    float scale = skullRadius;
    p /= scale;
    float d = drawSkull(p);
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

vec3 drawFinalBloom(vec3 p, float t, float scale) {
    float bt = smoothstep(0., 2., t);
    bt = easeOutCirc(bt);
    float bs = 1.2;
    Model blm = drawBloom(p / bs, bt);
    return vec3(blm.d * bs * scale, 0, 0);
}

vec3 skullWithBloom(inout vec3 p, inout float scale, inout float t) {
    
    if (t <= .0 || scale <= 0.) {
        return vec3(1e12, 0, 0);
    }

    // skull with sub blooms
    float d = drawSkullWithBlooms(p, t) * scale;
    vec3 res = vec3(d, 0, 0);
    
    // set location for next bloomWithSkull
    // this is the camera
    p -= bloomPosition;
    p /= stepScale;
    p *= bloomRotate;
    scale *= stepScale;
    t -= delay;

    vec3 bloom = drawFinalBloom(p, t, scale);
    res = opU(res, bloom);

    return res;
}

vec3 map(vec3 p) {

    float t = time;
    t += 1.;

    float camScale = tweenCamera(p, t);

    float w = mapCameraDebug(p);

    vec3 res = vec3(1e12, 0, 0);

    // return vec3((length(p) - .5) / camScale, 0., 0.);

    // draw bloom with skull (takes p, scale, time)
    //      updates p, scale, time
    //      loop

    float scale = 1.;
    vec3 res2;

    t *= delay;
    t += 1.;

    // 4 iterations
    // t += delay;
    // scale /= stepScale;
    // p *= inverse(bloomRotate);
    // p *= stepScale;
    // p += bloomPosition;

    // 3 iterations
    res = drawFinalBloom(p, t, scale);

    for (float i = 0.; i < 4.; i++) {
        tweenSkull(p, scale, t);
        res2 = skullWithBloom(p, scale, t);
        res = opU(res, res2);
    }

    // res.x = min(res.x, w);
    res.x /= camScale;

    return res;
}

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    vec3 npos;
    for (int i = 0; i < NORMAL_STEPS; i++){
        npos = pos + eps * invert;
        nor += map(npos).x * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

// #define AA 3

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    skullOffset = 1.8;
    skullRadius = .3;
    skullRotate = basisMatrix(vec3(.5,1,-.4), vec3(-.9,1.,0));
    skullRotate = basisMatrix(vec3(.9,1,.9), vec3(1,0,1));

    bloomPosition = vec3(.8,.8,-.1) * skullRadius;
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

    float mTime = mod(iTime/2., 1.);
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

        vec3 camPos = eye;
        vec3 rayDirection = normalize(dir);

        // mat3 camMat = calcLookAtMatrix( camPos, vec3(0,.23,-.35), -1.68);
        // rayDirection = normalize( camMat * vec3(p.xy,2.8) );

        vec3 rayPosition = camPos;
        float rayLength = 0.;
        float dist = 0.;
        bool bg = false;
        vec3 res;
        const float MAX_DIST = 100.;

        for (int i = 0; i < 300; i++) {
            rayLength += dist;
            rayPosition = camPos + rayDirection * rayLength;
            res = map(rayPosition);
            dist = res.x;

            if (abs(dist) < .00001) {
                break;
            }
            
            if (rayLength > MAX_DIST) {
                bg = true;
                break;
            }
        }

        vec3 bgCol = vec3(.19,.19,.22) * 1.3;
        col = bgCol;
        
        if ( ! bg) {
            vec3 nor = calcNormal(rayPosition);
            col = nor * .5 + .5;
            col *= clamp(dot(nor, vec3(1,1,0)) * .5 + .5, 0., 1.);
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
