precision highp float;

uniform vec2 iResolution;
uniform vec4 iMouse;
uniform float iTime;
uniform sampler2D iChannel0; // images/bubbles.png filter: linear wrap: repeat

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

const float EXP = 2.;
const float REP = 3.5;

float sinout(float t) {
    return sin(t * PI / 2.);
}

float shroom(vec3 p, float t) {
    float d = 1e12;
    float tf = floor(t);
    float s = pow(EXP, tf);
    if (tf < 1.) {
        s = (pow(EXP, t) - 1.) / (EXP - 1.);
        //s = smoothstep(0., 1., s);
        //s = sinout(s);
    }
    pR(p.xz, tf * 2. * PI / REP - .8);
    pR(p.yx, .6);
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
    uv *= 2.;
    uv = uv * .5 + .5;
    //return vec4(uv, 0, 1);
    return texture2D(iChannel0, uv);
}

struct Result {
    float dist;
    int material;
    vec3 albedo;
};

Result map(vec3 p) {
    float d = 1e12;
    int material = 0;    
    vec3 albedo = vec3(0);
    
    float zoom = pow(EXP, fTime);
    p /= zoom;
    pR(p.xz, fTime * 2. * PI / REP);
    vec3 pp = p;
    float sz = 0.;
    float h;
    float s;
    for (float i = 0.; i < 8.; i++) {
       d = min(d, shroom(p, i + fTime));
    }

    vec4 tex, texB;
    float tr, trB;
    
    for (float i = 0.; i < 12.; i++) {
        p /= 4.;
        pR(p.xz, i * -2. * PI / REP);
        texB = floorTex(p.xz * pow(EXP, i));
        texB.a = 1. / pow(EXP, i);
        float blend = .5;
        tr = 1. / pow(EXP, i - blend);
        trB = 1. / pow(EXP, i + blend);
        tex = mix(tex, texB, smoothstep(tr, trB, length(p.xz)));
        p = pp;
    }

    albedo = tex.rgb;
    p.y -= tex.r * .5 * tex.a;
    float ground = p.y * .5;
    if (ground < d) {
        d = ground;
        material = 1;
    }
    d *= zoom;
    return Result(d, material, albedo);
}

vec3 calcNormal(vec3 p) {
    vec2 e = vec2(1, -1) * .001;
    return normalize(
        e.xyy * map(p + e.xyy).dist +
        e.yyx * map(p + e.yyx).dist +
        e.yxy * map(p + e.yxy).dist +
        e.xxx * map(p + e.xxx).dist
    );
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = (-iResolution.xy + 2. * fragCoord) / iResolution.y;
    vec2 im = (iMouse.xy / iResolution.xy) * 2. - 1.;
    vec3 camPos = vec3(
        cos(im.x * PI) * .4,
        (im.y + 1.) * 2.,
        sin(im.x * PI) * .4
    );
    mat3 cam = camMat(camPos, vec3(0,.2,0), 0.);
    vec3 rd = cam * normalize(vec3(uv, 1.8));

    vec3 pos;
    Result res;
    float dist = 0.;
    float len = 0.;
    bool bg = true;
    
    for (int i = 0; i < 200; i++) {
        len += dist;
        pos = camPos + len * rd;
        res = map(pos);
        dist = res.dist;
        
        if (dist < .001) {
            bg = false;
            break;
        }
        
        if (len > 5.) {
            break;
        }
    }
    
    vec3 col = vec3(0);
    
    if ( ! bg) {
        col = calcNormal(pos) * .5 + .5;
        if (res.material == 1) {
            col *= res.albedo.r;
        }
    }
    
    // Output to screen
    fragColor = vec4(col,1.0);
}
