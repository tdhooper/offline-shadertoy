precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

// --------------------------------------------------------
// Spectrum colour palette
// IQ https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}



mat2 inverse(mat2 m) {
  return mat2(m[1][1],-m[0][1],
             -m[1][0], m[0][0]) / (m[0][0]*m[1][1] - m[0][1]*m[1][0]);
}


const float PI  = 3.14159265359;
const float PHI = 1.61803398875;

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}


float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
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

// Cylinder standing upright on the xz plane
float fCylinder(vec3 p, float r, float height) {
    float d = length(p.xz) - r;
    d = max(d, abs(p.y) - height);
    return d;
}

// Capsule: A Cylinder with round caps on both sides
float fCapsule(vec3 p, float r, float c) {
    c -= r;
    // return fCylinder(p, r, c);
    return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
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

float bloom(vec3 p) {
    float d = 1e12;
    float part;
    p.y -= .1;
    vec3 pp = p;
    float a, j;
    vec3 s;
    const float n = 40.;
    for (float i = 1.; i < n; i ++) {
        a = i * PHI * PI * 2.;
        p = pp;
        //p.xz -= vec2(sin(a), cos(a)) * i * .005;
        j = pow(i/n, 1.5);
        p.y += j * .2;
        pR(p.xz, -a);
        pR(p.yz, mix(.3, .8, i/n));
        s = vec3(.05,.05,.01) * mix(.1, 1., i/n);
        s.y = mix(.01, .15, i/n);
        p.y -= s.y / 2.;
        part = fBox(p, s - s.z) - s.z;
        d = min(d, part);
    }
    return d;
}

vec2 round(vec2 a) {
    return floor(a + .5);
}

vec2 fib(vec2 n) {
    vec2 uv = vec2(5., 8.);
    float aa = atan(uv.x / uv.y);
    float r = (PI*2.) / sqrt(uv.x*uv.x + uv.y*uv.y);
    //n.y = min(n.y, 1.);
    //n.y = max(n.y, .7);
    pR(n, aa);
    vec2 cell = round(n / r);
    n = cell * r;    
    pR(n, -aa);

    return n;
}

float bloomHeight = 1.;
float bloomHeightMax = 1.;

float leafBound(vec3 p, vec2 uv) {
    float d = abs(length(p) - uv.y) - .16;
    //return d;
    //d = max(d, p.y);
    pR(p.xz, -uv.x);
   // vec3 n = normalize(vec3(1,0,.5));
    
    float width = mix(.5, .1, min(uv.y, 1.));
    width = .75 / uv.y;
    vec3 n = normalize(vec3(1,0,width));

    d = max(d, -dot(p, n));
    d = max(d, dot(p, n * vec3(1,1,-1)));
    return d;
}

float debugLeaf(vec3 p, vec2 uv) {

    float bound = length(p + vec3(0, bloomHeight, 0)) - bloomHeight * 3.;

    //return bound;

    float r = circleFlatRadius(uv.y, bloomHeight);
    if (r == 0.) {
        return 1e12;
    }
    p.y += uv.y - r;
    float d2 = abs(length(p) - abs(r)) - .05;

    float d = abs(length(p) - uv.y) - .01;
    // d = min(d, d2);
    d = d2;
    d = max(d, bound);
    pR(p.xz, -uv.x);
    d = max(d, abs(p.x) - .05);


    return d;
}

float newLeaf(vec3 p, vec2 uv) {
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

float leafOld(vec3 p, vec2 uv) {
    float thick = clamp(uv.y, .7, 1.);
    thick = 1.;
    float th = thick * .16;
    pR(p.xz, -uv.x);
    //d = max(d, abs(p.x) - .05);
    float width = mix(.5, .1, min(uv.y, 1.));
    width = .75 / uv.y;
    width *= thick;
    vec3 n = normalize(vec3(1,0,width));
    float d = -dot(p, n);
    d = max(d, dot(p, n * vec3(1,1,-1)));
    float len = mix(PI / 1.2, PI / 2., pow(uv.y/2.9, 2.));
    len = max(len, 0.);
    d = smax(d, dot(p, vec3(0,sin(len),cos(len))), thick);
    d = smax(d, abs(length(p) - uv.y) - thick * th, th);
    //vec3 n = vec3(sin(uv.x + PI/2.), 0, cos(uv.x + PI/2.));
    //d = max(d, abs(dot(p, n)) - .05);
    p.z -= uv.y;
    //d = min(d, length(p) - .1);
    return d;
}


// need to calculate appear time
// then just subtract from total time

float tLeaf;

float leaf(vec3 p, vec2 uv) {
    float d = 1e12;
    // orient
    pR(p.xz, -uv.x);
    pR(p.yz, uv.y);

    p.z -= .5;
    return length(p) - .2;

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

    // cell.y = min(cell.y, -.5/stretch); // clamp
    cell.y = max(cell.y, (-6.)/stretch); // clamp

    cell = transform * cell; // warp
    cell = round(cell); // snap
    cell *= scale; // move into real units
    cell = transformI * cell; // remove warp
    cell += move; // offset
    return cell;
}


vec3 bloom2(vec3 p) {

    float bound = length(p - vec3(0,-1.2,0)) - 3.3;
    bound = max(bound, p.y - 1.1);
    if (bound > .01) {
        // return vec2(bound, 0.);
    }

    float t = iTime / 2.;
    t = mod(t, 1.);
    // t = smoothstep(0., .7, t) - pow(rangec(.7, 1., t), 2.);

    vec2 move = vec2(0, t);
    float stretch = mix(.5, 5., t);

    // move *= 0.;

    vec2 uv = vec2(
        atan(p.x, p.z),
        atan(-p.y, length(p.xz))
    );

    vec2 uuu = uv;

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

    uv -= move;
    uv = transform * uv;
    vec2 cell = round(uv / scale);

    float d = 1e12;

    d = min(d, leaf(p, calcCell(cell, vec2(-1, 0), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(0, -1), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(0, 0), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(1, -1), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(1, 0), transform, transformI, scale, move, stretch)));

    d = min(d, leaf(p, calcCell(cell, vec2(-1, -1), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(-1, 1), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(0, 1), transform, transformI, scale, move, stretch)));
    d = min(d, leaf(p, calcCell(cell, vec2(1, 1), transform, transformI, scale, move, stretch)));

    return vec3(d, 0, 0);
}

vec3 map(vec3 p) {
    p.y -= .5;
    return bloom2(p);
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

            if (abs(dist) < .001) {
                break;
            }
            
            if (rayLength > 30.) {
                bg = true;
                break;
            }
        }

        col =  vec3(.19,.19,.22) * .5;
        
        if ( ! bg) {
            vec3 nor = calcNormal(rayPosition);
            col = nor * .5 + .5;
            if (res.z == 1.) {
                col = spectrum(res.y);
                col *= clamp(dot(nor, vec3(1,1,0)), 0., 1.) * .5 + .5;
            }
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
