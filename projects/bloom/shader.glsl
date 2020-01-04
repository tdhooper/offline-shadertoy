precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
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

float map(vec3 p) {
    float d = length(p) - .5;
    p.y -= .5;
    //d = min(d, bloom(p));
    d = bloom(p);
    return d;
}

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    vec3 npos;
    for (int i = 0; i < NORMAL_STEPS; i++){
        npos = pos + eps * invert;
        nor += map(npos) * eps * invert;
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

#define AA 3

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

        for (int i = 0; i < 300; i++) {
            rayLength += dist;
            rayPosition = camPos + rayDirection * rayLength;
            dist = map(rayPosition);

            if (abs(dist) < .001) {
                break;
            }
            
            if (rayLength > 10.) {
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
