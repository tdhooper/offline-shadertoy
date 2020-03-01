
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

float drawSkull(vec3 p) {
    float d = length(p) - 1.;
    p.x = abs(p.x);
    d = smax(d, -(length(p - vec3(.4,.5,.6)) - .2), .3);
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

void calcSkullAnim(float t, inout float skullHeight, inout float skullScale) {
    t = clamp(t, 0., 1.);
    skullHeight = mix(.2, skullOffset, almostIdentityInv(rangec(.35, .7, t)));
    skullScale = mix(.0, 1., almostIdentityInv(rangec(.35, .5, t)));
}

vec3 bloomWithSkull(inout vec3 p, inout float scale, inout float t) {
    
    if (t <= 0.) {
        return vec3(1e12, 0, 0);
    }

    // bloom
    float bt = smoothstep(0., .6, t);
    Model blm = drawBloom(p, bt);
    vec3 res = vec3(blm.d, 0, 0);
    res.x *= scale;

    float skullHeight;
    float skullScale;
    calcSkullAnim(t, skullHeight, skullScale);
    p.y -= skullHeight;
    p *= skullRotate;

    if (skullScale > 0.) {
        p /= skullScale;
        scale *= skullScale;

        // skull with sub blooms
        float skd = drawSkullWithBlooms(p, t);
        vec3 sk = vec3(skd, 0, 0);
        sk.x *= scale;
        
        res = opU(res, sk);
    }

    // set location for next bloomWithSkull
    // this is the camera
    p -= bloomPosition;
    p /= stepScale;
    p *= bloomRotate;
    scale *= stepScale;
    t -= delay;

    return res;
}

vec3 map(vec3 p) {

    // return vec3(length(p) - .5, 0., 0.);

    float t = time;
    t += 1.;

    float camScale = tweenCamera(p, t);

    float w = mapCameraDebug(p);

    vec3 res = vec3(1e12, 0, 0);

    // draw bloom with skull (takes p, scale, time)
    //      updates p, scale, time
    //      loop

    float scale = 1.;
    vec3 res2;

    t *= delay;

    t += 1.;

    // draw skull (with blooms)
    // translate
    // draw bloom

    for (float i = 0.; i < 4.; i++) {
        res2 = bloomWithSkull(p, scale, t);
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

    skullOffset = 1.;
    skullRadius = .4;
    skullRotate = basisMatrix(vec3(-.0,0,1), vec3(0,1,-1));

    bloomPosition = normalize(vec3(1,.2,-.3)) * skullRadius * .8;
    bloomRotate = basisMatrix(cross(vec3(0,-1.,.5), bloomPosition), bloomPosition);

    stepPosition = vec3(0,skullOffset,0) + skullRotate * bloomPosition;
    stepScale = .2;
    stepRotate = skullRotate * bloomRotate;

    // position 2  =  position  +  rotation * position

    delay = 1.;

    cameraPrecalc();
    calcPhyllotaxis();

    vec3 col;
    vec3 tot = vec3(0.0);

    float mTime = mod(iTime/3., 1.);
    time = mTime;
    // time = 1.;

    vec2 o = vec2(0);

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

        for (int i = 0; i < 300; i++) {
            rayLength += dist;
            rayPosition = camPos + rayDirection * rayLength;
            res = map(rayPosition);
            dist = res.x;

            if (abs(dist) < .00001) {
                break;
            }
            
            if (rayLength > 100.) {
                bg = true;
                break;
            }
        }

        col =  vec3(.19,.19,.22) * .5;
        
        if ( ! bg) {
            vec3 nor = calcNormal(rayPosition);
            col = nor * .5 + .5;
            col *= clamp(dot(nor, vec3(1,1,0)) * .5 + .5, 0., 1.);
        }

        tot += col;
    #ifdef AA
    }
    tot /= float(AA*AA);
    #endif

    col = tot;
    col = pow( col, vec3(0.4545) );
    fragColor = vec4(col,1.0);
}
