precision highp float;

uniform vec2 iResolution;
uniform vec4 iMouse;
uniform float iTime;
uniform sampler2D iChannel0; // images/bubbles.png filter: linear wrap: repeat
uniform sampler2D iChannel1; // buffer-b.glsl filter: linear wrap: clamp

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

#define fTime mod(iTime / 3., 1.)

#define PI 3.14159

mat3 camMat(vec3 pos, vec3 tar, float roll) {
    vec3 w = normalize(tar - pos);
    vec3 p = vec3(sin(roll), cos(roll), 0);
    vec3 u = normalize(cross(w, p));
    vec3 v = cross(u, w);
    return mat3(u, v, w);
}

void pR(inout vec2 p, float r) {
    p = p * cos(r) + sin(r) * vec2(p.y, -p.x);
}


float range(float vmin, float vmax, float value) {
    return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float sinout(float t) {
    return sin(t * PI / 2.);
}

const float EXP = 2.;
const float EXPLOG = 1. / log(EXP);
const float REP = 3.5;

float shroom(vec3 p, float t) {
    float d = 1e12;
    float height = range(0., 3., t) * .2;
    float width = smoothstep(0., 1.5, t) * .075;
    p -= vec3(.1,0,0);
    pR(p.yx, .5);
    p.y -= height;
    d = min(d, max(length(p.xz) - width / 2., max(p.y, -p.y - height*2.)));
    float flatten = mix(1., 2., range(1.5, 2.5, t));
    flatten = 1.;
    d = min(d, max(length(p + vec3(0,(flatten-1.)*width*.9,0)) - width*flatten, -p.y));
    return d;
}

float shroom2(vec3 p, float t) {
    float d = 1e12;
    float height = range(1.7, 4., t) * .075;
    float width = smoothstep(1.7, 3., t) * .02;
    p -= vec3(.07,0,-.05);
    pR(p.yx, -.2);
    p.y -= height;
    d = min(d, max(length(p.xz) - width / 2., max(p.y, -p.y - height*2.)));
    float flatten = mix(1., 2., range(1.5, 2.5, t));
    flatten = 1.;
    d = min(d, max(length(p + vec3(0,(flatten-1.)*width*.9,0)) - width*flatten, -p.y));
    return d;
}

float shroom3(vec3 p, float t) {
    float d = 1e12;
    float height = smoothstep(-.3, 5., t) * .15;
    float width = smoothstep(-.3, 3.5, t) * .05;
    p -= vec3(.08,0,-.1);
    pR(p.yx, .1);
    p.y -= height;
    d = min(d, max(length(p.xz) - width / 2., max(p.y, -p.y - height*2.)));
    float flatten = mix(1., 2., range(1.5, 2.5, t));
    flatten = 1.;
    d = min(d, max(length(p + vec3(0,(flatten-1.)*width*.9,0)) - width*flatten, -p.y));
    return d;
}

struct Result {
    float dist;
    int material;
    vec3 albedo;
};


Result floorRes(vec3 p) {
    vec2 uv = p.xz;
    uv *= .75;
    uv.x -= 1.5;
    uv = uv * .5 + .5;
    //return vec4(uv, 0, 1);
    vec4 tex = texture2D(iChannel0, uv);
    p.y -= tex.r * 1.5 * (1./4.);
    float d = p.y * .5;

    return Result(d, 1, tex.rgb);
}

Result mix(Result a, Result b, float t) {
    int mat = t < .5 ? a.material : b.material;
    return Result(
        mix(a.dist, b.dist, t),
        mat,
        mix(a.albedo, b.albedo, t)
    );
}

Result expFloor(vec3 p) {
    vec2 uv = p.xz;
    vec3 pp = p;
    Result res, resA, resB;

    float i = EXPLOG * log(1. / length(uv));

    float ia = floor(i);
    float powia = pow(EXP, ia);
    pR(p.xz, ia * -2. * PI / REP);
    resA = floorRes(p * powia);
    resA.dist /= powia;

    p = pp;

    float ib = ceil(i);
    float powib = pow(EXP, ib);
    pR(p.xz, ib * -2. * PI / REP);
    resB = floorRes(p * powib);
    resB.dist /= powib;

    float blend = .2;
    blend = .5 - blend;
    res = mix(resA, resB, smoothstep(ia + blend, ib - blend, i));

    return res;
}

vec3 camPos;

Result map(vec3 p) {
    float camHole = length(p - camPos) - .35;

    float sp = length(p) - .5;

    float d = 1e12;
    int material = 0;    
    vec3 albedo = vec3(.15);
    
    float ceiling = p.y - 1.;

    float zoom = pow(EXP, fTime);
    p /= zoom;
    pR(p.xz, fTime * 2. * PI / REP);
    vec3 pp = p;
    float sc;
    float part;
    for (float i = 0.; i < 4.; i++) {
        sc = pow(EXP, i);
        p /= sc;
        pR(p.xz, i * 2. * PI / REP - .8);
        part = shroom(p, i + fTime) * sc;
        d = min(d, part);
        part = shroom2(p, i + fTime) * sc;
        d = min(d, part);
        part = shroom3(p, i + fTime) * sc;
        d = min(d, part);
        p = pp;
    }

    //d = max(d, ceiling);
    Result result = Result(d, material, albedo);

    Result floor = expFloor(p);
    if (floor.dist < result.dist) {
        result = floor;
    }

    result.dist *= zoom;
    result.dist = max(result.dist, -camHole);
    return result;
}

vec3 calcNormal(vec3 p) {
    vec2 e = vec2(1, -1) * .00001;
    return normalize(
        e.xyy * map(p + e.xyy).dist +
        e.yyx * map(p + e.yyx).dist +
        e.yxy * map(p + e.yxy).dist +
        e.xxx * map(p + e.xxx).dist
    );
}

float calcOcclusion(vec3 pos, vec3 nor)
{
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float h = 0.01 + 0.11*float(i)/4.0;
        vec3 opos = pos + h*nor;
        float d = map( opos ).dist;
        occ += (h-d)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 2.0*occ, 0.0, 1.0 );
}

// // http://iquilezles.org/www/articles/rmshadows/rmshadows.htm
// float calcSoftshadow( in vec3 ro, in vec3 rd)
// {
//     float r = 100.;
//     float res = 1.0;
//     float tmax = 12.0;
//     float t = 0.02;
//     for( int i=0; i<50; i++ )
//     {
//         float h = map( ro + rd * t).dist;
//         res = min( res, mix(1.0,16.0*h/t, 1.) );
//         t += clamp( h, 0.05, 0.40 );
//         if( res<0.005 || t>tmax ) break;
//     }
//     return clamp( res, 0.0, 1.0 );
// }

float calcSoftshadow( in vec3 ro, in vec3 rd)
{
    float tmax = 3.;
    int technique = 0;
    float res = 1.0;
    float t = .01;
    float ph = 1e10; // big, such that y = 0 on the first iteration
    float speed = 1.;
    
    for( int i=0; i<64; i++ )
    {
        float h = map( ro + rd * t ).dist;

        // traditional technique
        if( technique==0 )
        {
            res = min( res, 10.0*h/t );
        }
        // improved technique
        else
        {
            // use this if you are getting artifact on the first iteration, or unroll the
            // first iteration out of the loop
            //float y = (i==0) ? 0.0 : h*h/(2.0*ph); 

            float y = h*h/(2.0*ph);
            float d = sqrt(h*h-y*y);
            res = min( res, 10.0*d/max(0.0,t-y) );
            ph = h;
        }
        
        t += h * speed;
        
        if( res<0.01 || t>tmax ) break;
        
    }
    return clamp( res, 0.0, 1.0 );
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = (-iResolution.xy + 2. * fragCoord) / iResolution.y;
    vec2 im = (iMouse.xy / iResolution.xy) * 2. - 1.;
    camPos = vec3(
        cos(im.x * PI) * .4,
        (im.y + .5) * .5,
        sin(im.x * PI) * .4
    ) * .35 * 10.;
    mat3 cam = camMat(camPos, vec3(0,.05,0)*10., 0.);
    vec3 rd = cam * normalize(vec3(uv, 1.8));

    camPos = eye;
    rd = normalize(dir);

    vec3 pos;
    Result res;
    float dist = 0.;
    float len = 0.;
    bool bg = false;
    const float MAX_DIST = 10.;
    
    for (int i = 0; i < 200; i++) {
        len += dist;
        pos = camPos + len * rd;
        res = map(pos);
        dist = res.dist;
        
        if (dist < .00001) {
            break;
        }
        
        if (len > MAX_DIST) {
            bg = true;
            break;
        }
    }
    
    vec3 bgcol = vec3(.01,.01,0);
    vec3 col = bgcol;
    int mat;

    if ( ! bg) {
        col = res.albedo;
        mat = res.material;
        float spec = 2.;

        if (mat == 1) {
            col = vec3(.05) * res.albedo;
            spec = .0;
        }

        vec3 nor = calcNormal(pos) * .5 + .5;
        float occ = calcOcclusion(pos, nor);

        vec3  sun_lig = normalize(vec3(-.0, 1, -.0));
        vec3  sun_hal = normalize( sun_lig-rd );
        float sun_dif = clamp(dot(nor, sun_lig), 0., 1.);
        float sun_spe = spec * pow(clamp(dot(nor,sun_hal),0.0,1.0),8.0)*sun_dif*(0.04+0.96*pow(clamp(1.0+dot(sun_hal,rd),0.0,1.0),5.0));
        float sun_sha = calcSoftshadow(pos, sun_lig);

        float sky_dif = sqrt(clamp( 0.5+0.5*nor.y, 0.0, 1.0 ));

        float bou_dif = sqrt(clamp( 0.1-0.9*nor.y, 0.0, 1.0 ))*clamp(1.0-0.1*pos.y,0.0,1.0);

        vec3 lin = vec3(0.);
        lin += sun_dif*vec3(8.10,6.00,4.20)*vec3(sun_sha,sun_sha*sun_sha*0.5+0.5*sun_sha,sun_sha*sun_sha);
        lin += sky_dif*vec3(0.50,0.70,1.00)*occ;
        lin += bou_dif*vec3(0.20,0.70,0.10)*occ;
        col = col * lin;
        col += sun_spe*vec3(9.90,8.10,6.30)*sun_sha;
        col = mix(col, bgcol, 1.0-exp( -0.01*pow(len, 3.) ) );
    }



    // Output to screen
    fragColor = vec4(col, len / MAX_DIST);
}
