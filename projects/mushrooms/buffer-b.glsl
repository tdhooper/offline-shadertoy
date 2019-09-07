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
    return vec3(d.y - d.x, idx);
    //return vec3(d.x, idx);
    //return max(d.y*.91 - d.x*1.1, 0.)/.91;
    // return sqrt(d.y) - sqrt(d.x); // etc.
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec3 v = voronoi(uv * 5.);
    float h = v.x * step(hash21(v.yz), .5);
    fragColor = vec4(h, 0, 0, 1);
}
