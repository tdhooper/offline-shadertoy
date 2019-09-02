precision highp float;

uniform vec2 iResolution;
uniform vec4 iMouse;
uniform float iTime;
uniform sampler2D iChannel0; // images/bubbles.png filter: linear wrap: repeat

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
    float tf = floor(t);
    float s = pow(EXP, tf);
    s = mix(0., s, (range(0., 5., t)));
    if (tf < 2.) {
        //s = (pow(EXP, t) - 1.) / (EXP - 1.);
        //s = smoothstep(0., 1., s);
        //s = sinout(s);
    }
    pR(p.xz, tf * 2. * PI / REP - .8);
    pR(p.yx, .5);
    float h = s * .05;
    p.x -= s * .02;
    p.y -= h;
    float sz = s * .024;
    d = min(d, max(length(p.xz) - sz / 2., max(p.y, -p.y - h*2.)));
    float flatten = mix(1., 2., range(1.5, 2.5, t));
    flatten = 1.;
    d = min(d, max(length(p + vec3(0,(flatten-1.)*sz*.9,0)) - sz*flatten, -p.y));
    return d;
}

vec4 floorTex(vec2 uv) {
    uv *= .75;
    uv.x -= 1.5;
    uv = uv * .5 + .5;
    //return vec4(uv, 0, 1);
    return texture2D(iChannel0, uv);
}

vec4 expFloorTex(vec2 uv) {
    vec2 uva = uv;
    vec4 tex, texA, texB;

    float i = EXPLOG * log(1. / length(uv));

    float ia = floor(i);
    float powia = pow(EXP, ia);
    pR(uv, ia * -2. * PI / REP);
    texA = floorTex(uv * powia);
    texA.a = 1. / powia;

    uv = uva;

    float ib = ceil(i);
    float powib = pow(EXP, ib);
    pR(uv, ib * -2. * PI / REP);
    texB = floorTex(uv * powib);
    texB.a = 1. / powib;

    float blend = .2;
    blend = .5 - blend;
    tex = mix(texA, texB, smoothstep(ia + blend, ib - blend, i));
    tex.a /= 4.;
    return tex;
}

struct Result {
    float dist;
    int material;
    vec3 albedo;
};

Result map(vec3 p) {
    float d = 1e12;
    int material = 0;    
    vec3 albedo = vec3(.15);
    
    float ceiling = p.y - 1.;

    float zoom = pow(EXP, fTime);
    p /= zoom;
    pR(p.xz, fTime * 2. * PI / REP);
    vec3 pp = p;
    float sz = 0.;
    float h;
    float s;
    for (float i = 0.; i < 4.; i++) {
       d = min(d, shroom(p, i + fTime));
       pR(p.xz, PI / REP * -.5);
       p = pp;
    }

    //d = max(d, ceiling);

    vec4 tex = expFloorTex(p.xz);

    p.y -= tex.r * 1.5 * tex.a;
    float ground = p.y * .5;
    if (ground < d) {
        d = ground;
        material = 1;
        albedo = tex.rgb;
    }
    d *= zoom;
    return Result(d, material, albedo);
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
    float tmax = 1.;
    int technique = 0;
    float res = 1.0;
    float t = .02;
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
    vec3 camPos = vec3(
        cos(im.x * PI) * .4,
        (im.y + .5) * .5,
        sin(im.x * PI) * .4
    ) * .35;
    mat3 cam = camMat(camPos, vec3(0,.05,0), 0.);
    vec3 rd = cam * normalize(vec3(uv, 1.8));

    camPos = eye;
    rd = normalize(dir);

    vec3 pos;
    Result res;
    float dist = 0.;
    float len = 0.;
    bool bg = false;
    
    for (int i = 0; i < 200; i++) {
        len += dist;
        pos = camPos + len * rd;
        res = map(pos);
        dist = res.dist;
        
        if (dist < .00001) {
            break;
        }
        
        if (len > 10.) {
            bg = true;
            break;
        }
    }
    
    vec3 col = vec3(0);
    int mat;

    if ( ! bg) {
        col = res.albedo;
        mat = res.material;

        if (mat == 1) {
            col = vec3(.05) * res.albedo;
        }

        vec3 nor = calcNormal(pos) * .5 + .5;
        float occ = calcOcclusion(pos, nor);

        vec3  sun_lig = normalize(vec3(-.3, 1, .3));
        float sun_dif = clamp(dot(nor, sun_lig), 0., 1.);
        float sun_sha = calcSoftshadow(pos, sun_lig);

        float sky_dif = sqrt(clamp( 0.5+0.5*nor.y, 0.0, 1.0 ));

        float bou_dif = sqrt(clamp( 0.1-0.9*nor.y, 0.0, 1.0 ))*clamp(1.0-0.1*pos.y,0.0,1.0);

        vec3 lin = vec3(0.);
        lin += sun_dif*vec3(8.10,6.00,4.20)*vec3(sun_sha,sun_sha*sun_sha*0.5+0.5*sun_sha,sun_sha*sun_sha);
        lin += sky_dif*vec3(0.50,0.70,1.00)*occ;
        lin += bou_dif*vec3(0.20,0.70,0.10)*occ;
        col = col * lin;
    }

    col = pow(col, vec3(0.4545));

    // Output to screen
    fragColor = vec4(col,1.0);
}
