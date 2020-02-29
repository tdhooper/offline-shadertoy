
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

float skullOffset = 1.5;
float skullRadius = .4;
vec3 bloomPosition;
float delay = .7;

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

float leaf(vec3 p, vec2 uv, float bloomHeight, float bloomHeightMax) {
    float r = circleFlatRadius(uv.y, bloomHeight);
    if (r == 0.) {
        return 1e12;
    }

    // orient
    pR(p.xz, -uv.x);

    // wedge
    vec3 n = normalize(vec3(1,0,.75));
    float wedge = -dot(p, n);
    wedge = max(wedge, dot(p, n * vec3(1,1,-1)));

    float e = uv.y / bloomHeightMax;
    e = 1. - abs(1. - e);

    // sphere
    float thick = mix(.01, .1, pow(e * bloomHeightMax, .5));
    p.y += uv.y - r;
    float sphere = abs(length(p) - abs(r)) - thick;

    // top
    float len = 1. - e;
    len = sqrt(1. - len * len);
    float a = (10. * len) / (2. * PI * r);
    a = min(a, PI);
    n = vec3(0, sin(a), cos(a));
    float top = dot(p, n);

    float round = mix(-1.5, 1.5, len);
    round = max(0., round);
    // round = 0.;
    float d = smax(wedge, top, round);
    d = smax(d, sphere, thick);

    // 
    // pR(p.xz, -uv.x);
    // d = max(d, abs(p.x) - .05);
    return d;
}

vec2 calcCell(
    vec2 cell,
    vec2 offset,
    mat2 transform,
    mat2 transformI,
    float scale,
    vec2 move,
    float stretch
) {
    cell += offset;
    cell = transformI * cell; // remove warp
    cell.y = min(cell.y, -.5/stretch); // clamp
    cell.y = max(cell.y, -.9); // clamp
    cell = transform * cell; // warp
    cell = round(cell); // snap
    cell *= scale; // move into real units
    cell = transformI * cell; // remove warp
    cell += move; // offset
    return cell;
}


// stepPosition needs to set the evential skull position for the camera
// skullWithBloom needs the bloom position, that results in the skull position

// is it still practical to put the skull transform in bloom?

float calcSkullOffset(float t) {
    float tt = clamp(t, 0., 1.);
    tt = almostIdentityInv(tt);
    tt *= 1.5;
    
    float bloomHeightMax = 1.;
    float bloomHeight = mix(.1, bloomHeightMax, tt);;
    return bloomHeight;
}

vec3 bloom(vec3 p, float t) {

    float bloomHeightMax = skullOffset;

    t = clamp(t, 0., 1.);
    t = almostIdentityInv(t);

    float bloomHeight = mix(.1, bloomHeightMax, t);
    p.y -= bloomHeight;

    vec2 move = vec2(0, t) * skullOffset;
    float stretch = 5.;

    // move *= 0.;

    vec2 uv = vec2(
        atan(p.x, p.z),
        circleFlat(vec2(-p.y, length(p.xz)), bloomHeight)
    );

    vec2 uuu = uv;

    uv -= move;

    vec2 cc = vec2(5., 8.);
    //cc.y += floor(sin(iTime * .5) * 3.);
    float aa = atan(cc.x / cc.y);
    //float aa = 0.5585993153435624;
    float scale = (PI*2.) / sqrt(cc.x*cc.x + cc.y*cc.y);
    // scale /= 2.;
    //float scale = 0.6660163105297472;
    mat2 mRot = mat2(cos(aa), -sin(aa), sin(aa), cos(aa));
    mat2 mScale = mat2(1,0,0,stretch);
    mat2 transform = mRot * mScale;
    mat2 transformI = inverse(transform);

    uv = transform * uv;
    vec2 cell = round(uv / scale);

    float d = 1e12;

    d = min(d, leaf(p, calcCell(cell, vec2(-1, 0), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(0, -1), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(0, 0), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(1, -1), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(1, 0), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));

    d = min(d, leaf(p, calcCell(cell, vec2(-1, -1), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(-1, 1), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(0, 1), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));
    d = min(d, leaf(p, calcCell(cell, vec2(1, 1), transform, transformI, scale, move, stretch), bloomHeight, bloomHeightMax));

    // d = min(d, sk);

    return vec3(d, 0, 0);
}


void applyMat4(inout vec3 p, mat4 m) {
    p = (vec4(p, 1) * m).xyz;
}

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

// vec3 skullWithBloom(
//     vec3 p,
//     float t,
//     inout vec3 nextP,
//     inout float nextScale,
//     inout float nextT
// ) {

//     // skull
//     float d = length(p) - .4;
//     d = fBox(p, vec3(.05, .3, .1));
    
//     // bloom

//     float bl = bloom(p, t, nextP, nextScale).x * stepScale;
//     d = min(d, part);
//     p -= stepPosition;
//     p /= stepScale;
//     p *= stepRotate;
//     scale /= stepScale;


//     p.y += calcSkullOffset(1.) * .1;
//     // p.y += .1;
//     t -= delay;
//     // if (t > 0.) {
//         p /= stepScale;
//         float bl = bloom(p, t, nextP, nextScale).x * stepScale;
//         nextScale *= stepScale;
//         d = min(d, bl);
//     // }

//     nextT = t;

//     return vec3(d, 0, 0);
// }

// vec3 skull(vec3 p, float t) {
//     float d = length(p) - .4 * smoothstep(.4, .73, t);
//     return vec3(d, 0, 0);
// }

vec3 opU(vec3 a, vec3 b) {
    return a.x < b.x ? a : b;
}

void calcSkullAnim(float t, inout float skullHeight, inout float skullScale) {
    float bloomHeightMax = skullOffset;
    t = clamp(t, 0., 1.);
    t = almostIdentityInv(t);
    float bloomHeight = mix(.1, bloomHeightMax, t);
    skullHeight = bloomHeight;
    skullScale = mix(.1, 1., smoothstep(.0, 1., t));
}

vec3 bloomWithSkull(inout vec3 p, inout float scale, inout float t) {
    
    if (t <= 0.) {
        return vec3(1e12, 0, 0);
    }

    // bloom
    vec3 bl = bloom(p, t);
    bl.x *= scale;

    float skullHeight;
    float skullScale;
    calcSkullAnim(t, skullHeight, skullScale);
    p.y -= skullHeight;
    p /= skullScale;
    scale *= skullScale;

    // skull with sub blooms
    vec3 sk = vec3(length(p) - skullRadius, 0, 0);
    sk.x *= scale;
    
    vec3 res = opU(bl, sk);

    // set location for next bloomWithSkull
    // this is the camera
    p -= bloomPosition;
    p /= stepScale;
    p *= stepRotate;
    scale *= stepScale;
    t -= delay;

    return res;
}

vec3 map(vec3 p) {

    // return vec3(length(p) - .5, 0., 0.);

    float t = iTime / 3.;
    t = mod(t, 1.);

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

    skullOffset = 1.5;
    skullRadius = .4;
    bloomPosition = normalize(vec3(1,1,1)) * skullRadius;

    stepPosition = vec3(0,skullOffset,0) + bloomPosition;
    stepScale = .1;
    stepRotate = basisMatrix(cross(vec3(0,0,1), bloomPosition), bloomPosition);

    cameraPrecalc();

    vec3 col;
    vec3 tot = vec3(0.0);

    float mTime = mod(iTime / 1., 1.);
    time = mTime;

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
