precision highp float;

uniform vec2 iResolution;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

// shane - https://www.shadertoy.com/view/ldtGWj
vec2 hash22(vec2 p) { 
    float n = sin(dot(p, vec2(41, 289))); 
    return fract(vec2(8, 1)*262144.*n);
}

float hash21(vec2 p) {
    return fract(1e4 * sin(17.0 * p.x + p.y * 0.1) * (0.1 + abs(sin(p.y * 13.0 + p.x))));
}

// shane - https://www.shadertoy.com/view/ldtGWj
vec3 voronoi(in vec2 p){
    vec2 g = floor(p), o; p -= g;
    vec3 d = vec3(1.); // 1.4, etc. "d.z" holds the distance comparison value.
    vec2 cid;
    vec2 idx;
    for(int y = -1; y <= 1; y++){
        for(int x = -1; x <= 1; x++){
            
            o = vec2(x, y);
            cid = g + o;
            o += hash22(cid) - p;
            
            d.z = dot(o, o);
            d.y = max(d.x, min(d.y, d.z));
            if (d.x > d.z) {
                idx = cid;
                d.x = d.z;
            }

        }
    }
    float r;
    r = d.y - d.x, idx;
    //r = d.x;
    //r = max(d.y*.91 - d.x*1.1, 0.)/.91;
    r = sqrt(d.y) - sqrt(d.x);
    return vec3(r, idx);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    //uv *= 2.;
    uv.x -= 1.;
    vec3 v0 = voronoi((uv + vec2(0.)) * 5.) * .6;
    vec3 v1 = voronoi((uv + vec2(1.)) * 10.) * .3;
    vec3 v2 = voronoi((uv + vec2(2.)) * 25.) * .075;
    vec3 v3 = voronoi((uv + vec2(3.)) * 60.) * .025;

    vec3 v = (
        v0 * 1.
        //v1 * .3 * step(hash21(v1.yz), .5)
       // v1 * .1 * step(hash21(v2.yz), .5)
    );

    float on = step(hash21(v0.yz), .5);
    v *= on;

    vec2 dir = normalize(hash22(v.yz + 1.) * 2. - 1.);
    uv += (sin(uv.x * 60.) * sin(uv.y * 30.)) * .01;
    float grain = mod((sin(dot(uv, dir) * 5.) * .5 + .5) * 100., 1.);
    grain = smoothstep(0., .5, grain) - smoothstep(.5, 1., grain);

    v.x *= mix(.95, 1., grain);
    //v *= (.3 + v2 * .7).x;

    float h = v.x * 3.;
    //h = v.x * hash21(v.yz);
    fragColor = vec4(h, on, grain, 1);
}
