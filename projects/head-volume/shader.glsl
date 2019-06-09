precision highp float;

uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform vec2 iChannel0Size;
// uniform float iTime;

varying vec3 eye;
varying vec3 dir;


// filter: nearest, linear, mipmap
// wrap: clamp, repeat

// Fork of "tdhpr-sdf-volume-head" by tdhooper. https://shadertoy.com/view/wlXGWN
// 2019-04-27 01:52:35

// Fork of "tdhpr-sdf-volume-2" by tdhooper. https://shadertoy.com/view/wtXGWN
// 2019-04-27 01:11:30

// Fork of "tdhpr-sdf-volume" by tdhooper. https://shadertoy.com/view/wtX3D4
// 2019-04-27 01:06:42


float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmin(vec3 v) {
    return min(min(v.x, v.y), v.z);
}

float vmin(vec2 v) {
    return min(v.x, v.y);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float mHead(vec3 p) {
    vec3 pa = p;
    float bound = fBox(p, vec3(.45,.65,.6));
    #ifdef MIRROR
        p.x = -abs(p.x);
    #endif
    p += OFFSET / SCALE;
    bound = fBox(p, 1./SCALE);
    //return bound;
    if (bound > .01) {
        return bound;
    }
    //p.x = -abs(p.x);
    //p += OFFSET / SCALE;
    p *= SCALE;
    float d = mapTex(iChannel0, p, iChannel0Size);
    //return min(d, max(bound, pa.x));
    return d;
    return min(d, bound + .02);
}

float map(vec3 p) {
    // return length(p) - .5;
    p.y -= .15;
    //pR(p.yz, .2);
    // pR(p.xz, iTime/2. + .4);
    // pR(p.yz, iTime/2. + .4);
    float d = mHead(p);
   // d = mix(d, fBox(p, vec3(.7)), sin(iTime) * .5+ .5);
    return d;
  //  vec2 uv = spaceToTex(p);
//    return texture2D(iChannel0, uv).r;
    return length(p) - .5;
}

bool isDebug = false;

float mapDebug(vec3 p) {
    float d = map(p);
    return d;
    float r = min(abs(p.z), min(abs(p.x), abs(p.y-.05))) - .001;
    if (r < d) {
        isDebug = true;
        return r;
    } else {
        isDebug = false;
    }
    return d;

}

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    for (int i = 0; i < NORMAL_STEPS; i++){
        nor += map(pos + eps * invert) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}


vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;
    
    // vec3 space = texToSpace(fragCoord.xy, 0, iResolution);
    // // fragColor = vec4(space, 1); return;
    // // if (p.x < .9) {fragColor = vec4(spectrum(1.), 1); return;}
    // // fragColor = vec4(spectrum(space.z * .5 + .5), 1); return;

    // vec3 tex = spaceToTex(space, iResolution);
    // tex.b /= 4.;
    // // fragColor = vec4(vec3(step(tex.x, iTime)), 1); return;
    // // fragColor = vec4(vec3(tex.z), 1); return;
    // fragColor = vec4(vec3(tex), 1); return;
    // // fragColor = vec4(spectrum(tex.z), 1); return;


    // vec3 camPos = vec3(0,.05,3.2) * .5;
    // vec3 rayDirection = normalize(vec3(p + vec2(0,-0),-4));

    // float r2 = .0;//iTime;
    // pR(camPos.yz, r2);
    // pR(rayDirection.yz, r2);

    // float r = .5;//iTime + .7;
    // pR(camPos.xz, r);
    // pR(rayDirection.xz, r);

    vec3 camPos = eye;
    vec3 rayDirection = dir;

    vec3 rayPosition = camPos;
    float rayLength = 0.;
    float dist = 0.;
    bool bg = false;
    vec3 col = vec3(0);

    for (int i = 0; i < 300; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        dist = mapDebug(rayPosition);

        if (abs(dist) < .001) {
            break;
        }
        
        if (rayLength > 10.) {
            bg = true;
            break;
        }
    }
    
    if ( ! bg) {
        vec3 n = calcNormal(rayPosition);
        col = n * .5 + .5;
        
        if (isDebug) {
            float d = map(rayPosition);
            col = vec3(mod(d * 10., 1.));
        }
    }
    
  //  col = vec3(spaceToTex(vec3(-1,-1,-.7)), 0.);

    //vec2 uv = fragCoord.xy / iResolution.xy;
    //vec3 ps = texToSpace(uv)[3].xyz;
    //col = abs(ps);
    //col = vec3(texture2D(iChannel0, uv).a);
    
    fragColor = vec4(col,1.0);
}