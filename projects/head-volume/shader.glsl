precision highp float;

uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform float iTime;

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
    p.x = -abs(p.x);
    p += OFFSET / SCALE;
    bound = fBox(p, 1./SCALE);
    //return bound;
    if (bound > .01) {
        return bound;
    }
    //p.x = -abs(p.x);
    //p += OFFSET / SCALE;
    p *= SCALE;
    float d = mapTex(iChannel0, p);
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

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;
    
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