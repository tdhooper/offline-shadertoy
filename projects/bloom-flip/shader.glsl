precision highp float;

uniform vec2 iResolution;
uniform float iTime;

uniform sampler2D iChannel0; // /images/noise.png

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#extension GL_EXT_shader_texture_lod : enable

// Reference image https://images.squarespace-cdn.com/content/v1/5968af67414fb590cb8f77e3/1503430627560-ORAR051BSQFDS3PL0LZ2/ke17ZwdGBToddI8pDm48kL3VKmwKI3leYB51VJjLFB8UqsxRUqqbr1mOJYKfIPR7LoDQ9mXPOjoJoqy81S2I8N_N4V1vUb5AoIIIbLZhVYxCRW4BPu10St3TBAUQYVKcgK5SGg9Ovb1yloBBOHcruw_mYLfAhRzzgArFCB07Dw0L8n4JypuoE5Tg6Wg5Oyvs/Echeveria-peacockii3.jpg?format=2500w
// https://rareplant.me/cacti-succulents/echeveria-peacockii-subsessilis

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


vec2 hash2( vec2 p )
{
    // texture based white noise
    return texture2DLodEXT( iChannel0, (p+0.5)/256.0, 0.).xy;
    
    // procedural white noise   
    //return fract(sin(vec2(dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3))))*43758.5453);
}

// https://www.shadertoy.com/view/ldl3W8
float voronoi( in vec2 x )
{
    vec2 cell = floor(x);

    float d = 1e12;
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        vec2 offset = vec2(float(i),float(j));
        vec2 pos = hash2( cell + offset );
        // #ifdef ANIMATE
        // o = 0.5 + 0.5*sin( iTime + 6.2831*o );
        // #endif  
        vec2 r = cell + offset + pos;
        d = min(d, length(x - r));
    }

    return d;
}


mat2 inverse(mat2 m) {
  return mat2(m[1][1],-m[0][1],
             -m[1][0], m[0][0]) / (m[0][0]*m[1][1] - m[0][1]*m[1][0]);
}


const float PI  = 3.14159265359;
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

float fCapsule(vec3 p, float r, float c) {
    return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
}

float time;

vec2 round(vec2 a) {
    return floor(a + .5);
}

vec4 leaf(vec3 p, vec3 cellData) {
    vec2 cell = cellData.xy;
    float cellTime = cellData.z;

    float d = 1e12;
    float d2 = 1e12;
    float slice = 1e12;
    float wedge, wedges;

    // orient
    pR(p.xz, -cell.x);
    pR(p.zy, cell.y);

    vec3 pp = p;

    cellTime = max(cellTime, 0.);

    float core = length(p) - .1;

    float len = max(cellTime*3. - .2, 0.);
    len = pow(len, .33);
    float llen = len;


    if (cellTime > 0.) {
        // p.x /= 3.;

        // wedge
        float ins = .25;
        p.z += ins;
        vec3 n = normalize(vec3(1,0,.35));
        wedge = -dot(p, n);
        wedge = max(wedge, dot(p, n * vec3(1,1,-1)));
        wedge = smax(wedge, p.z - len*1.12 - ins, len);
        p.z -= ins;

        // wedge2
        ins = .2;
        p.z += ins;
        n = normalize(vec3(1,0,.4));
        float wedge2 = -dot(p, n);
        wedge2 = max(wedge2, dot(p, n * vec3(1,1,-1)));
        wedge2 = smax(wedge2, p.z - len*.95 - ins, len*.6);
        p.z -= ins;

        float r = len / 8.;
        d = fCapsule(p.xzy, r, len);

        float top = p.y - len * .7;
        float curve = smoothstep(0., .2, cellTime);
        // curve = 0.;
        len *= mix(1.5, .65, curve);
        pR(p.zy, -mix(.2, .7, curve));
        slice = length(p - vec3(0,len,0)) - len;
        d2 = abs(slice) - .05;
        
        float d3 = smax(d2, wedge, .05);
        float d4 = smax(d2, wedge2, .05);
        wedges = smin(wedge, wedge2, .01);
        d3 = smin(d3, d4, .01);
        d3 = max(d3, top);

        // d = min(d, d2);
        d = d3;
    }

    vec3 col = vec3(.15,.15,.4);

    if (d < .01) {

        p = pp;
        len = llen;

        vec2 uv = p.xz / len;

        float v = voronoi((uv+4.)*30.);
        float v2 = voronoi((uv+4.)*4.+cell.x);

        // d = smin(d, core, .05);

        col = mix(col, vec3(.125,.2,.4), 1.-v2);
        float tip = length(p - vec3(0,.2,len*.9));
        // d = min(d, tip - .1);

        tip = smoothstep(.5, .0, tip);
        tip *= smoothstep(.07, .0, abs(slice+.01));
        tip *= smoothstep(-.2, .0, wedges);
        tip = pow(tip, 1.5);
        col = mix(col, vec3(1,.2,.5), tip);

        float vs = 1.-uv.y*1.;
        vs *= smoothstep(.0, -.1, wedges);
        vs *= smoothstep(.0, .05, abs(slice));
        v = smoothstep(vs + .1, vs - .5, v*1.5);
        col = mix(col, vec3(.05,.05,.2), v*v2);

        // col = mix(col, vec3(.45,.7,.6), smoothstep(.1, 1.8, cell.y));
        col = mix(col, vec3(.6,1.,.9), smoothstep(.1, 1.8, cell.y) * .66);
    }

    // col = vec3(mod(uv, .5) / .5, 0);

    return vec4(d, col);
}

vec3 calcCellData(
    vec2 cell,
    vec2 offset,
    float maxBloomOffset,
    mat2 transform,
    mat2 transformI,
    float stretch,
    float stretchStart,
    float stretchEnd,
    float t
) {

    float sz = maxBloomOffset + PI / 2.;

    cell = transform * cell;

    // Snap to cell center
    cell = round(cell);
    cell += offset;

    // Hide leaves outside the growth area
    cell = transformI * cell;
    cell.y *= stretch / sz / stretchStart;
    cell.y = max(cell.y, .5/stretchStart); // clamp, not sure why this magic number
    cell.y /= stretch / sz / stretchStart;
    cell = transform * cell;

    // Snap after clamp
    cell = round(cell);

    cell = transformI * cell;

    // calculate cell time
    float y = cell.y * (stretch / sz);
    float cellAppearTime = (stretchStart - y) / (stretchStart - stretchEnd);
    float cellTime = t - cellAppearTime;

    cell.y -= maxBloomOffset;

    return vec3(cell, cellTime);
}

vec4 opU(vec4 a, vec4 b) {
    return a.x < b.x ? a : b;
}

vec4 bloom2(vec3 p, float t) {

    p.y -= .05;

    // float bound = -p.y-.3;
    // bound = max(bound, length(p) - 2.);
    // if (bound > .001) {
    //    return vec3(bound, 0, 0); 
    // }

    // t = rangec(-.2, 1., t);

    vec2 move = vec2(0, t);
    float stretchStart = .25;
    float stretchEnd = 1.;
    float stretch = mix(stretchStart, stretchEnd, t);
    float maxBloomOffset = PI / 2.;


    vec2 cell = vec2(
        atan(p.x, p.z),
        atan(p.y, length(p.xz)) + maxBloomOffset
    );

    vec2 cc = vec2(5., 8.);
    float aa = atan(cc.x / cc.y);
    //float aa = 0.5585993153435624;
    float scale = (PI*2.) / sqrt(cc.x*cc.x + cc.y*cc.y);
    //float scale = 0.6660163105297472;
    mat2 mRot = mat2(cos(aa), -sin(aa), sin(aa), cos(aa));
    mat2 mScale = mat2(1,0,0,stretch);
    mat2 mScale2 = mat2(1./scale,0,0,1./scale);
    mat2 transform = mRot * mScale * mScale2;
    mat2 transformI = inverse(transform);

    vec4 res = vec4(1e12, 0, 0, 0);

    res = opU(res, leaf(p, calcCellData(cell, vec2(-1, 0), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(0, -1), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(0, 0), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(1, -1), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(1, 0), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));

    res = opU(res, leaf(p, calcCellData(cell, vec2(-1, -1), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(-1, 1), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(0, 1), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));
    res = opU(res, leaf(p, calcCellData(cell, vec2(1, 1), maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t)));

    return res;
}

vec4 map(vec3 p) {
    float t;

    // p.x += time * .5;

    
    pR(p.xy, time * -PI);

    vec3 pp = p;


    t = time + .5;
    t = sin(t * PI - PI/2.) * .5 + .5;
    pR(p.xz, time * PI);
    vec4 res = bloom2(p, t);

    p = pp;
    p.y *= -1.;
    p.z *= -1.;

    t = time - .5;
    t = sin(t * PI - PI/2.) * .5 + .5;
    pR(p.xz, time * PI);
    vec4 res2 = bloom2(p, t);
    res = opU(res, res2);
    // res = res2;
    
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

    vec3 col;
    vec3 tot = vec3(0.0);

    float mTime = mod(iTime / 2., 1.);
    time = mTime;
    // time = sin(time * PI * 2. - PI/2.) * .5 + .5;

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

        // float vv = voronoi((p * 1. + 2.) * 2.);
        // vv = step(vv, .5);
        // fragColor = vec4(vec3(vv), 1); return;

        vec3 camPos = eye;
        vec3 rayDirection = normalize(dir);

        // mat3 camMat = calcLookAtMatrix( camPos, vec3(0,.23,-.35), -1.68);
        // rayDirection = normalize( camMat * vec3(p.xy,2.8) );

        vec3 rayPosition = camPos;
        float rayLength = 0.;
        float dist = 0.;
        bool bg = false;
        vec4 res;

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

        col = vec3(.18,.24,.36);
        
        if ( ! bg) {

            vec3 lightpos = vec3(1,1,0);

            vec3 nor = calcNormal(rayPosition);
            vec3 viewdir = normalize(camPos - rayPosition);
            vec3 lightdir = normalize(lightpos - rayPosition);

            col = nor * .5 + .5;
            col = res.yzw;
            // if (res.z == 1.) {
            //     col = spectrum(res.y);
            //     // col *= res.y > 0. && res.y < 1. ? 1. : .2;
            //     // col *= mod(res.y, 1.);
            // }
            col *= clamp(dot(nor, vec3(1,1,0)), 0., 1.) * .5 + .5;
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
