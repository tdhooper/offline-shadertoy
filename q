[1mdiff --git a/index.js b/index.js[m
[1mindex 724faf3..282576d 100644[m
[1m--- a/index.js[m
[1m+++ b/index.js[m
[36m@@ -24,7 +24,8 @@[m [mconst main = require('./main.js');[m
 // head-4k[m
 // peel[m
 // plode-2[m
[32m+[m[32m// multipass[m
 [m
[31m-const project = LOADPROJECT('multipass');[m
[32m+[m[32mconst project = LOADPROJECT('head-volume');[m
 [m
 main(project);[m
[1mdiff --git a/main.js b/main.js[m
[1mindex c58bde9..39f07b1 100644[m
[1m--- a/main.js[m
[1m+++ b/main.js[m
[36m@@ -8,8 +8,10 @@[m [mconst regl = require('regl')({[m
     'webgl_depth_texture',[m
     'ext_frag_depth',[m
     'oes_standard_derivatives',[m
[32m+[m[32m    'oes_texture_float',[m
[32m+[m[32m    'oes_texture_float_linear',[m
   ],[m
[31m-  pixelRatio: .5,[m
[32m+[m[32m  // pixelRatio: .5,[m
   // pixelRatio: 1,[m
   attributes: {[m
     preserveDrawingBuffer: true,[m
[36m@@ -49,6 +51,7 @@[m [mmodule.exports = (project) => {[m
   stats.dom.classList.add('stats');[m
 [m
   const canvas = regl._gl.canvas;[m
[32m+[m[32m  const gl = regl._gl;[m
 [m
   const renderNodes = buildRenderNodes(shaders);[m
 [m
[36m@@ -57,9 +60,10 @@[m [mmodule.exports = (project) => {[m
       node.buffer = regl.framebuffer({[m
         width: 1024,[m
         height: 1024,[m
[32m+[m[32m        colorType: 'float',[m
       });[m
     }[m
[31m-    node.draw = regl({[m
[32m+[m[32m    const d = regl({[m
       frag: node.shader,[m
       uniforms: node.dependencies.reduce((acc, dep) => {[m
         acc[dep.uniform] = dep.node.buffer;[m
[36m@@ -67,6 +71,16 @@[m [mmodule.exports = (project) => {[m
       }, {}),[m
       framebuffer: node.buffer,[m
     });[m
[32m+[m[32m    node.draw = (state) => {[m
[32m+[m[32m      node.dependencies.forEach((dep) => {[m
[32m+[m[32m        const texture = dep.node.buffer.color[0]._texture;[m
[32m+[m[32m        gl.activeTexture(gl.TEXTURE0);[m
[32m+[m[32m        gl.bindTexture(texture.target, texture.texture);[m
[32m+[m[32m        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);[m
[32m+[m[32m        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);[m
[32m+[m[32m      });[m
[32m+[m[32m      d(state);[m
[32m+[m[32m    };[m
   });[m
 [m
   const setup = regl({[m
[1mdiff --git a/projects/head-volume/buffer-a.glsl b/projects/head-volume/buffer-a.glsl[m
[1mnew file mode 100644[m
[1mindex 0000000..3ff503d[m
[1m--- /dev/null[m
[1m+++ b/projects/head-volume/buffer-a.glsl[m
[36m@@ -0,0 +1,532 @@[m
[32m+[m[32mprecision highp float;[m
[32m+[m
[32m+[m[32m#define PI 3.14159265359[m
[32m+[m
[32m+[m[32mvoid pR(inout vec2 p, float a) {[m
[32m+[m[32m    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mvec2 pRi(vec2 p, float a) {[m
[32m+[m[32m    pR(p, a);[m
[32m+[m[32m    return p;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32m#define saturate(x) clamp(x, 0., 1.)[m
[32m+[m
[32m+[m[32mfloat vmax(vec2 v) {[m
[32m+[m[32m    return max(v.x, v.y);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat vmax(vec3 v) {[m
[32m+[m[32m    return max(max(v.x, v.y), v.z);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat vmin(vec3 v) {[m
[32m+[m[32m    return min(min(v.x, v.y), v.z);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat vmin(vec2 v) {[m
[32m+[m[32m    return min(v.x, v.y);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat fBox(vec3 p, vec3 b) {[m
[32m+[m[32m    vec3 d = abs(p) - b;[m
[32m+[m[32m    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat fCorner2(vec2 p) {[m
[32m+[m[32m    return length(max(p, vec2(0))) + vmax(min(p, vec2(0)));[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat fDisc(vec3 p, float r) {[m
[32m+[m[32m    float l = length(p.xz) - r;[m
[32m+[m[32m    return l < 0. ? abs(p.y) : length(vec2(p.y, l));[m
[32m+[m[32m}[m
[32m+[m
[32m+[m
[32m+[m[32mfloat fHalfCapsule(vec3 p, float r) {[m
[32m+[m[32m    return mix(length(p.xz) - r, length(p) - r, step(0., p.y));[m
[32m+[m[32m}[m
[32m+[m
[32m+[m
[32m+[m[32m// IQ https://www.shadertoy.com/view/Xds3zN[m
[32m+[m[32mfloat sdRoundCone( in vec3 p, in float r1, float r2, float h )[m
[32m+[m[32m{[m
[32m+[m[32m    vec2 q = vec2( length(p.xz), p.y );[m
[32m+[m[41m    [m
[32m+[m[32m    float b = (r1-r2)/h;[m
[32m+[m[32m    float a = sqrt(1.0-b*b);[m
[32m+[m[32m    float k = dot(q,vec2(-b,a));[m
[32m+[m[41m    [m
[32m+[m[32m    if( k < 0.0 ) return length(q) - r1;[m
[32m+[m[32m    if( k > a*h ) return length(q-vec2(0.0,h)) - r2;[m
[32m+[m[41m        [m
[32m+[m[32m    return dot(q, vec2(a,b) ) - r1;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat smin2(float a, float b, float r) {[m
[32m+[m[32m    vec2 u = max(vec2(r - a,r - b), vec2(0));[m
[32m+[m[32m    return max(r, min (a, b)) - length(u);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat smax2(float a, float b, float r) {[m
[32m+[m[32m    vec2 u = max(vec2(r + a,r + b), vec2(0));[m
[32m+[m[32m    return min(-r, max (a, b)) + length(u);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat smin(float a, float b, float k){[m
[32m+[m[32m    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);[m
[32m+[m[32m    return (1. - f) * a + f  * b - f * (1. - f) * k;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat smax(float a, float b, float k) {[m
[32m+[m[32m    return -smin(-a, -b, k);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat smin3(float a, float b, float k){[m
[32m+[m[32m    return min([m
[32m+[m[32m        smin(a, b, k),[m
[32m+[m[32m        smin2(a, b, k)[m
[32m+[m[32m    );[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat smax3(float a, float b, float k){[m
[32m+[m[32m    return max([m
[32m+[m[32m        smax(a, b, k),[m
[32m+[m[32m        smax2(a, b, k)[m
[32m+[m[32m    );[m
[32m+[m[32m}[m
[32m+[m
[32m+[m
[32m+[m[32m// Modelling[m
[32m+[m
[32m+[m[32mfloat ellip(vec3 p, vec3 s) {[m
[32m+[m[32m    float r = vmin(s);[m
[32m+[m[32m    p *= r / s;[m
[32m+[m[32m    return length(p) - r;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat ellip(vec2 p, vec2 s) {[m
[32m+[m[32m    float r = vmin(s);[m
[32m+[m[32m    p *= r / s;[m
[32m+[m[32m    return length(p) - r;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat helix(vec3 p, float lead, float thick) {[m
[32m+[m[32m    // p.z += iTime * .1;[m
[32m+[m[32m    float d = (mod(atan(p.y, p.x) - p.z * lead, PI * 2.) - PI) / lead;[m
[32m+[m[32m    d = abs(d) - thick;[m
[32m+[m[32m    return d;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mvoid fMouth(inout float d, vec3 pp) {[m
[32m+[m[32m    vec3 p;[m
[32m+[m[32m    // mouth base[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.0,.29,-.29);[m
[32m+[m[32m    pR(p.yz, -.3);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.13,.15,.1)), .18);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.37,-.4);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.03,.03,.02) * .5), .1);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.09,.37,-.31);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.04)), .18);[m
[32m+[m
[32m+[m[32m    // bottom lip[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.455,-.455);[m
[32m+[m[32m    p.z += smoothstep(.0, .2, p.x) * .05;[m
[32m+[m[32m    float lb = mix(.035, .03, smoothstep(.05, .15, length(p)));[m
[32m+[m[32m    vec3 ls = vec3(.055,.028,.022) * 1.25;[m
[32m+[m[32m    float w = .192;[m
[32m+[m[32m    vec2 pl2 = vec2(p.x, length(p.yz * vec2(.79,1)));[m
[32m+[m[32m    float bottomlip = length(pl2 + vec2(0,w-ls.z)) - w;[m
[32m+[m[32m    bottomlip = smax(bottomlip, length(pl2 - vec2(0,w-ls.z)) - w, .055);[m
[32m+[m[32m    d = smin(d, bottomlip, lb);[m
[32m+[m[41m    [m
[32m+[m[32m    // top lip[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.38,-.45);[m
[32m+[m[32m    pR(p.xz, -.3);[m
[32m+[m[32m    ls = vec3(.065,.03,.05);[m
[32m+[m[32m    w = ls.x * (-log(ls.y/ls.x) + 1.);[m
[32m+[m[32m    vec3 pl = p * vec3(.78,1,1);[m
[32m+[m[32m    float toplip = length(pl + vec3(0,w-ls.y,0)) - w;[m
[32m+[m[32m    toplip = smax(toplip, length(pl - vec3(0,w-ls.y,0)) - w, .065);[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.33,-.45);[m
[32m+[m[32m    pR(p.yz, .7);[m
[32m+[m[32m    float cut;[m
[32m+[m[32m    cut = dot(p, normalize(vec3(.5,.25,0))) - .056;[m
[32m+[m[32m    float dip = smin([m
[32m+[m[32m        dot(p, normalize(vec3(-.5,.5,0))) + .005,[m
[32m+[m[32m        dot(p, normalize(vec3(.5,.5,0))) + .005,[m
[32m+[m[32m        .025[m
[32m+[m[32m    );[m
[32m+[m[32m    cut = smax(cut, dip, .04);[m
[32m+[m[32m    cut = smax(cut, p.x - .1, .05);[m
[32m+[m[32m    toplip = smax(toplip, cut, .02);[m
[32m+[m
[32m+[m[32m    d = smin(d, toplip, .07);[m
[32m+[m
[32m+[m
[32m+[m[32m    // seam[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.425,-.44);[m
[32m+[m[32m    lb = length(p);[m
[32m+[m[32m    float lr = mix(.04, .02, smoothstep(.05, .12, lb));[m
[32m+[m[32m    pR(p.yz, .1);[m
[32m+[m[32m    p.y -= smoothstep(0., .03, p.x) * .002;[m
[32m+[m[32m    p.y += smoothstep(.03, .1, p.x) * .007;[m
[32m+[m[32m    p.z -= .133;[m
[32m+[m[32m    float seam = fDisc(p, .2);[m
[32m+[m[32m    seam = smax(seam, -d - .015, .01); // fix inside shape[m
[32m+[m[32m    d = mix(d, smax(d, -seam, lr), .65);[m
[32m+[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mbool isMap = true;[m
[32m+[m[32mbool isEye = false;[m
[32m+[m
[32m+[m[32mfloat mHead(vec3 p) {[m
[32m+[m
[32m+[m[32m    pR(p.yz, -.1);[m
[32m+[m[32m    //p.y -= .13;[m
[32m+[m
[32m+[m[32m    vec3 pa = p;[m
[32m+[m[32m    p.x = abs(p.x);[m
[32m+[m[32m    vec3 pp = p;[m
[32m+[m
[32m+[m[32m    float d = 1e12;[m
[32m+[m
[32m+[m[32m    // skull back[m
[32m+[m[32m    p += vec3(0,-.135,.09);[m
[32m+[m[32m    d = ellip(p, vec3(.395, .385, .395));[m
[32m+[m
[32m+[m[32m    // skull base[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,-.135,.09) + vec3(0,.1,.07);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.38, .36, .35)), .05);[m
[32m+[m
[32m+[m[32m    // forehead[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,-.145,-.175);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.315, .3, .33)), .18);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    pR(p.yz, -.5);[m
[32m+[m[32m    float bb = fBox(p, vec3(.5,.67,.7));[m
[32m+[m[32m    d = smax(d, bb, .2);[m
[32m+[m
[32m+[m[32m    // face base[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.25,-.13);[m
[32m+[m[32m    d = smin(d, length(p) - .28, .1);[m
[32m+[m
[32m+[m[32m    // behind ear[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.15,.13,.06);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.15,.15,.15)), .15);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.07,.18,.1);[m
[32m+[m[32m    d = smin(d, length(p) - .2, .18);[m
[32m+[m
[32m+[m[32m    // cheek base[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.2,.12,-.14);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.15,.22,.2) * .8), .15);[m
[32m+[m
[32m+[m[32m    // jaw base[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.475,-.16);[m
[32m+[m[32m    pR(p.yz, .8);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.19,.1,.2)), .1);[m
[32m+[m
[32m+[m[32m    // brow[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,-.0,-.18);[m
[32m+[m[32m    vec3 bp = p;[m
[32m+[m[32m    float brow = fHalfCapsule(p * vec3(.65,1,.9), .27);[m
[32m+[m[32m    brow = length(p) - .36;[m
[32m+[m[32m    p.x -= .37;[m
[32m+[m[32m    brow = smax(brow, dot(p, normalize(vec3(1,.2,-.2))), .2);[m
[32m+[m[32m    p = bp;[m
[32m+[m[32m    brow = smax(brow, dot(p, normalize(vec3(0,.6,1))) - .43, .25);[m
[32m+[m[32m    p = bp;[m
[32m+[m[32m    pR(p.yz, -.5);[m
[32m+[m[32m    float peak = -p.y - .165;[m
[32m+[m[32m    peak += smoothstep(.0, .2, p.x) * .01;[m
[32m+[m[32m    peak -= smoothstep(.12, .29, p.x) * .025;[m
[32m+[m[32m    brow = smax(brow, peak, .07);[m
[32m+[m[32m    p = bp;[m
[32m+[m[32m    pR(p.yz, .5);[m
[32m+[m[32m    brow = smax(brow, -p.y - .06, .15);[m
[32m+[m[32m    d = smin(d, brow, .06);[m
[32m+[m
[32m+[m[32m    // nose[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.03,-.45);[m
[32m+[m[32m    pR(p.yz, 3.);[m
[32m+[m[32m    d = smin(d, sdRoundCone(p, .008, .05, .18), .1);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.06,-.47);[m
[32m+[m[32m    pR(p.yz, 2.77);[m
[32m+[m[32m    d = smin(d, sdRoundCone(p, .005, .04, .225), .05);[m
[32m+[m
[32m+[m[32m    // jaw[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    vec3 jo = vec3(-.25,.4,-.07);[m
[32m+[m[32m    p = pp + jo;[m
[32m+[m[32m    float jaw = dot(p, normalize(vec3(1,-.2,-.05))) - .069;[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(.5,-.25,.35))) - .13, .12);[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(-.0,-1.,-.8))) - .12, .15);[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(.98,-1.,.15))) - .13, .08);[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(.6,-.2,-.45))) - .19, .15);[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(.5,.1,-.5))) - .26, .15);[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(1,.2,-.3))) - .22, .15);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.63,-.2);[m
[32m+[m[32m    pR(p.yz, .15);[m
[32m+[m[32m    float cr = .5;[m
[32m+[m[32m    jaw = smax(jaw, length(p.xy - vec2(0,cr)) - cr, .05);[m
[32m+[m
[32m+[m[32m    p = pp + jo;[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(0,-.4,1))) - .35, .1);[m
[32m+[m[32m    jaw = smax(jaw, dot(p, normalize(vec3(0,1.5,2))) - .3, .2);[m
[32m+[m[32m    jaw = max(jaw, length(pp + vec3(0,.6,-.3)) - .7);[m
[32m+[m
[32m+[m[32m    p = pa;[m
[32m+[m[32m    p += vec3(.2,.5,-.1);[m
[32m+[m[32m    float jb = length(p);[m
[32m+[m[32m    jb = smoothstep(.0, .4, jb);[m
[32m+[m[32m    float js = mix(0., -.005, jb);[m
[32m+[m[32m    jb = mix(.01, .04, jb);[m
[32m+[m
[32m+[m[32m    d = smin(d, jaw - js, jb);[m
[32m+[m
[32m+[m[32m    // chin[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.585,-.395);[m
[32m+[m[32m    p.x *= .7;[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.028,.028,.028)*1.2), .15);[m
[32m+[m
[32m+[m[32m    // return d;[m
[32m+[m
[32m+[m[32m    // cheek[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.2,.2,-.28);[m
[32m+[m[32m    pR(p.xz, .5);[m
[32m+[m[32m    pR(p.yz, .4);[m
[32m+[m[32m    float ch = ellip(p, vec3(.1,.1,.12)*1.05);[m
[32m+[m[32m    d = smin(d, ch, .1);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.26,.02,-.1);[m
[32m+[m[32m    pR(p.xz, .13);[m
[32m+[m[32m    pR(p.yz, .5);[m
[32m+[m[32m    float temple = ellip(p, vec3(.1,.1,.15));[m
[32m+[m[32m    temple = smax(temple, p.x - .07, .1);[m
[32m+[m[32m    d = smin(d, temple, .1);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(.0,.2,-.32);[m
[32m+[m[32m    ch = ellip(p, vec3(.1,.08,.1));[m
[32m+[m[32m    d = smin(d, ch, .1);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.17,.31,-.17);[m
[32m+[m[32m    ch = ellip(p, vec3(.1));[m
[32m+[m[32m    d = smin(d, ch, .1);[m
[32m+[m
[32m+[m[32m    fMouth(d, pp);[m
[32m+[m
[32m+[m[32m    // nostrils base[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.3,-.43);[m
[32m+[m[32m    d = smin(d, length(p) - .05, .07);[m
[32m+[m
[32m+[m[32m    // nostrils[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(0,.27,-.52);[m
[32m+[m[32m    pR(p.yz, .2);[m
[32m+[m[32m    float nostrils = ellip(p, vec3(.055,.05,.06));[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.043,.28,-.48);[m
[32m+[m[32m    pR(p.xy, .15);[m
[32m+[m[32m    p.z *= .8;[m
[32m+[m[32m    nostrils = smin(nostrils, sdRoundCone(p, .042, .0, .12), .02);[m
[32m+[m
[32m+[m[32m    d = smin(d, nostrils, .02);[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.033,.3,-.515);[m
[32m+[m[32m    pR(p.xz, .5);[m
[32m+[m[32m    d = smax(d, -ellip(p, vec3(.011,.03,.025)), .015);[m
[32m+[m
[32m+[m[32m    // return d;[m
[32m+[m
[32m+[m[32m    // eyelids[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.16,.07,-.34);[m
[32m+[m[32m    float eyelids = ellip(p, vec3(.08,.1,.1));[m
[32m+[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.16,.09,-.35);[m
[32m+[m[32m    float eyelids2 = ellip(p, vec3(.09,.1,.07));[m
[32m+[m
[32m+[m[32m    // edge top[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.173,.148,-.43);[m
[32m+[m[32m    p.x *= .97;[m
[32m+[m[32m    float et = length(p.xy) - .09;[m
[32m+[m
[32m+[m[32m    // edge bottom[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.168,.105,-.43);[m
[32m+[m[32m    p.x *= .9;[m
[32m+[m[32m    float eb = dot(p, normalize(vec3(-.1,-1,-.2))) + .001;[m
[32m+[m[32m    eb = smin(eb, dot(p, normalize(vec3(-.3,-1,0))) - .006, .01);[m
[32m+[m[32m    eb = smax(eb, dot(p, normalize(vec3(.5,-1,-.5))) - .018, .05);[m
[32m+[m
[32m+[m[32m    float edge = max(max(eb, et), -d);[m
[32m+[m
[32m+[m[32m    d = smin(d, eyelids, .01);[m
[32m+[m[32m    d = smin(d, eyelids2, .03);[m
[32m+[m[32m    d = smax(d, -edge, .005);[m
[32m+[m
[32m+[m[32m    // eyeball[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.165,.0715,-.346);[m
[32m+[m[32m    float eyeball = length(p) - .088;[m
[32m+[m[32m    if (isMap) isEye = eyeball < d;[m
[32m+[m[32m    d = min(d, eyeball);[m
[32m+[m
[32m+[m[32m    // tear duct[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.075,.1,-.37);[m
[32m+[m[32m    d = min(d, length(p) - .05);[m
[32m+[m
[32m+[m[41m    [m
[32m+[m[32m    // ear[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.405,.12,.10);[m
[32m+[m[32m    pR(p.xy, -.12);[m
[32m+[m[32m    pR(p.xz, .35);[m
[32m+[m[32m    pR(p.yz, -.3);[m
[32m+[m[32m    vec3 pe = p;[m
[32m+[m
[32m+[m[32m    // base[m
[32m+[m[32m    float ear = p.s + smoothstep(-.05, .1, p.y) * .015 - .005;[m
[32m+[m[32m    float earback = -ear - mix(.001, .025, smoothstep(.3, -.2, p.y));[m
[32m+[m
[32m+[m[32m    // inner[m
[32m+[m[32m    pR(p.xz, -.5);[m
[32m+[m[32m    float iear = ellip(p.zy - vec2(.01,-.03), vec2(.045,.05));[m
[32m+[m[32m    iear = smin(iear, length(p.zy - vec2(.04,-.09)) - .02, .09);[m
[32m+[m[32m    float ridge = iear;[m
[32m+[m[32m    iear = smin(iear, length(p.zy - vec2(.1,-.03)) - .06, .07);[m
[32m+[m[32m    ear = smax2(ear, -iear, .04);[m
[32m+[m[32m    earback = smin(earback, iear - .04, .02);[m
[32m+[m
[32m+[m[32m    // ridge[m
[32m+[m[32m    p = pe;[m
[32m+[m[32m    pR(p.xz, .2);[m
[32m+[m[32m    ridge = ellip(p.zy - vec2(.01,-.03), vec2(.045,.055));[m
[32m+[m[32m    ridge = smin3(ridge, -pRi(p.zy, .2).x - .01, .015);[m
[32m+[m[32m    ridge = smax3(ridge, -ellip(p.zy - vec2(-.01,.1), vec2(.12,.08)), .02);[m
[32m+[m
[32m+[m[32m    float ridger = .01;[m
[32m+[m
[32m+[m[32m    ridge = max(-ridge, ridge - ridger);[m
[32m+[m
[32m+[m[32m    ridge = smax2(ridge, abs(p.x) - ridger/2., ridger/2.);[m
[32m+[m
[32m+[m[32m    ear = smin(ear, ridge, .045);[m
[32m+[m
[32m+[m[32m    p = pe;[m
[32m+[m
[32m+[m[32m    // outline[m
[32m+[m[32m    float outline = ellip(pRi(p.yz, .2), vec2(.12,.09));[m
[32m+[m[32m    outline = smin(outline, ellip(p.yz + vec2(.155,-.02), vec2(.035, .03)), .14);[m
[32m+[m
[32m+[m[32m    // edge[m
[32m+[m[32m    float eedge = p.x + smoothstep(.2, -.4, p.y) * .06 - .03;[m
[32m+[m
[32m+[m[32m    float edgeo = ellip(pRi(p.yz, .1), vec2(.095,.065));[m
[32m+[m[32m    edgeo = smin(edgeo, length(p.zy - vec2(0,-.1)) - .03, .1);[m
[32m+[m[32m    float edgeoin = smax(abs(pRi(p.zy, .15).y + .035) - .01, -p.z-.01, .01);[m
[32m+[m[32m    edgeo = smax(edgeo, -edgeoin, .05);[m
[32m+[m
[32m+[m[32m    float eedent = smoothstep(-.05, .05, -p.z) * smoothstep(.06, 0., fCorner2(vec2(-p.z, p.y)));[m
[32m+[m[32m    eedent += smoothstep(.1, -.1, -p.z) * .2;[m
[32m+[m[32m    eedent += smoothstep(.1, -.1, p.y) * smoothstep(-.03, .0, p.z) * .3;[m
[32m+[m[32m    eedent = min(eedent, 1.);[m
[32m+[m
[32m+[m[32m    eedge += eedent * .06;[m
[32m+[m
[32m+[m[32m    eedge = smax(eedge, -edgeo, .01);[m
[32m+[m[32m    ear = smin(ear, eedge, .01);[m
[32m+[m[32m    ear = max(ear, earback);[m
[32m+[m
[32m+[m[32m    ear = smax2(ear, outline, .015);[m
[32m+[m
[32m+[m[32m    d = smin(d, ear, .015);[m
[32m+[m
[32m+[m[32m    // hole[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.36,.19,.06);[m
[32m+[m[32m    pR(p.xz, -.5);[m
[32m+[m[32m    pR(p.xy, -.2);[m
[32m+[m[32m    p.x += .02;[m
[32m+[m
[32m+[m[32m    // targus[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.34,.2,.02);[m
[32m+[m[32m    d = smin2(d, ellip(p, vec3(.015,.025,.015)), .035);[m
[32m+[m[32m    p = pp;[m
[32m+[m[32m    p += vec3(-.37,.18,.03);[m
[32m+[m[32m    pR(p.xz, .5);[m
[32m+[m[32m    pR(p.yz, -.4);[m
[32m+[m[32m    d = smin(d, ellip(p, vec3(.01,.03,.015)), .015);[m
[32m+[m[41m    [m
[32m+[m[32m    return d;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat map(vec3 p) {[m
[32m+[m[32m    p -= OFFSET;[m
[32m+[m[32m    p /= SCALE;[m
[32m+[m[32m    return mHead(p);[m
[32m+[m[32m    return length(p) - .3;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mvoid mainImage( out vec4 fragColor, in vec2 fragCoord )[m
[32m+[m[32m{[m
[32m+[m[32m    vec2 uv = fragCoord.xy / iResolution.xy;[m
[32m+[m[41m    [m
[32m+[m[32m    // vec4 lastFrame = texture2d(iChannel0, uv);[m
[32m+[m[32m    // if (lastFrame.x != 0. && iFrame > 2) {[m
[32m+[m[32m    //     fragColor = lastFrame;[m
[32m+[m[32m    //     return;[m
[32m+[m[32m    // }[m
[32m+[m[41m    [m
[32m+[m[32m    vec3 p0 = texToSpace(uv)[0].xyz;[m
[32m+[m[32m    vec3 p1 = texToSpace(uv)[1].xyz;[m
[32m+[m[32m    vec3 p2 = texToSpace(uv)[2].xyz;[m
[32m+[m[32m    vec3 p3 = texToSpace(uv)[3].xyz;[m
[32m+[m[41m    [m
[32m+[m[32m    fragColor = vec4([m
[32m+[m[32m        map(p0),[m
[32m+[m[32m        map(p1),[m
[32m+[m[32m        map(p2),[m
[32m+[m[32m        map(p3)[m
[32m+[m[32m    );[m
[32m+[m[32m}[m
\ No newline at end of file[m
[1mdiff --git a/projects/head-volume/common.glsl b/projects/head-volume/common.glsl[m
[1mnew file mode 100644[m
[1mindex 0000000..114bfc6[m
[1m--- /dev/null[m
[1m+++ b/projects/head-volume/common.glsl[m
[36m@@ -0,0 +1,91 @@[m
[32m+[m[32mprecision highp float;[m
[32m+[m
[32m+[m[32muniform vec2 iResolution;[m
[32m+[m
[32m+[m[32m// 640 x 360 buffer[m
[32m+[m[32m// 40 x 180 x 128 voxels[m
[32m+[m[32mvec2 texSubdivisions = vec2(16,2);[m
[32m+[m
[32m+[m[32m#define SCALE (vec3(4.1,1.73,1.75) * 1.)[m
[32m+[m[32m#define OFFSET vec3(.95, .094, -.088)[m
[32m+[m
[32m+[m
[32m+[m[32m// Divide texture into 3d space coordinates[m
[32m+[m[32m// uv = 2d texture coordinates 0:1[m
[32m+[m[32m// c = channel 0:3[m
[32m+[m
[32m+[m[32m// xy is split for each z[m
[32m+[m
[32m+[m[32m// Returns matrix representing three positions in space[m
[32m+[m[32m// vec3 p0 = mat4[0].xyz;[m
[32m+[m[32m// vec3 p1 = mat4[1].xyz;[m
[32m+[m[32m// vec3 p2 = mat4[2].xyz;[m
[32m+[m[32m// vec3 p3 = mat4[4].xyz;[m
[32m+[m
[32m+[m[32mvec3 texToSpace(vec2 uv, int c) {[m
[32m+[m[32m    vec2 sub = texSubdivisions;[m
[32m+[m[32m    uv *= sub;[m
[32m+[m[32m    float z = floor(uv.x) + floor(uv.y) * sub.x + float(c) * sub.x * sub.y;[m
[32m+[m[32m    z /= sub.x * sub.y * 4. - 1.;[m
[32m+[m[32m    uv = mod(uv, 1.);[m
[32m+[m[32m    vec3 p = vec3(uv, z);[m
[32m+[m[32m    p = p * 2. - 1.; // range -1:1[m
[32m+[m[32m    return p;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mmat4 texToSpace(vec2 uv) {[m
[32m+[m[32m    return mat4([m
[32m+[m[32m        vec4(texToSpace(uv, 0), 0),[m
[32m+[m[32m        vec4(texToSpace(uv, 1), 0),[m
[32m+[m[32m        vec4(texToSpace(uv, 2), 0),[m
[32m+[m[32m        vec4(texToSpace(uv, 3), 0)[m
[32m+[m[32m    );[m
[32m+[m[32m}[m
[32m+[m
[32m+[m
[32m+[m[32m// uv and channel[m
[32m+[m[32mvec3 spaceToTex(vec3 p) {[m
[32m+[m[32m    p = clamp(p, -1., 1.);[m
[32m+[m[32m    p = p * .5 + .5; // range 0:1[m
[32m+[m[32m    vec2 sub = texSubdivisions;[m
[32m+[m[32m    vec2 uv = p.xy;[m
[32m+[m[32m    //uv = clamp(uv, 0., 1.);[m
[32m+[m[32m    uv /= sub;[m
[32m+[m[32m    float i = floor(p.z * (sub.x * sub.y * 4. - 1.));[m
[32m+[m[32m    uv += vec2(mod(i, sub.x), mod(floor(i / sub.x), sub.y)) / sub;[m
[32m+[m[32m    float c = floor(i / (sub.x * sub.y));[m
[32m+[m[32m    return vec3(uv, c);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat range(float vmin, float vmax, float value) {[m
[32m+[m[32m  return clamp((value - vmin) / (vmax - vmin), 0., 1.);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat pickIndex(vec4 v, int i) {[m
[32m+[m[32m    if (i == 0) return v.r;[m
[32m+[m[32m    if (i == 1) return v.g;[m
[32m+[m[32m    if (i == 2) return v.b;[m
[32m+[m[32m    if (i == 3) return v.a;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat mapTex(sampler2D tex, vec3 p) {[m
[32m+[m[32m    // stop x bleeding into the next cell as it's the mirror cut[m
[32m+[m[32m    p.x = clamp(p.x, -.95, .95);[m
[32m+[m[32m    vec2 sub = texSubdivisions;[m
[32m+[m[32m    float zRange = (sub.x * sub.y * 4. - 1.) / 2.;[m
[32m+[m[32m    p.z += .5/zRange;[m
[32m+[m[32m    float zFloor = floor(p.z * zRange) / zRange;[m
[32m+[m[32m    float zCeil = ceil(p.z * zRange) / zRange;[m
[32m+[m[32m    vec3 uvcA = spaceToTex(vec3(p.xy, zFloor));[m
[32m+[m[32m    vec3 uvcB = spaceToTex(vec3(p.xy, zCeil));[m
[32m+[m[32m    float a = pickIndex(texture2D(tex, uvcA.xy), int(uvcA.z));[m
[32m+[m[32m    float b = pickIndex(texture2D(tex, uvcB.xy), int(uvcB.z));[m
[32m+[m[32m    return mix(a, b, range(zFloor, zCeil, p.z));[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mvoid mainImage(out vec4 a, in vec2 b);[m
[32m+[m
[32m+[m[32mvoid main() {[m
[32m+[m[32m    mainImage(gl_FragColor, gl_FragCoord.xy);[m
[32m+[m[32m}[m
[32m+[m
[1mdiff --git a/projects/head-volume/shader.glsl b/projects/head-volume/shader.glsl[m
[1mnew file mode 100644[m
[1mindex 0000000..a985804[m
[1m--- /dev/null[m
[1m+++ b/projects/head-volume/shader.glsl[m
[36m@@ -0,0 +1,160 @@[m
[32m+[m[32mprecision highp float;[m
[32m+[m
[32m+[m[32muniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp[m
[32m+[m[32muniform float iTime;[m
[32m+[m
[32m+[m[32m// filter: nearest, linear, mipmap[m
[32m+[m[32m// wrap: clamp, repeat[m
[32m+[m
[32m+[m[32m// Fork of "tdhpr-sdf-volume-head" by tdhooper. https://shadertoy.com/view/wlXGWN[m
[32m+[m[32m// 2019-04-27 01:52:35[m
[32m+[m
[32m+[m[32m// Fork of "tdhpr-sdf-volume-2" by tdhooper. https://shadertoy.com/view/wtXGWN[m
[32m+[m[32m// 2019-04-27 01:11:30[m
[32m+[m
[32m+[m[32m// Fork of "tdhpr-sdf-volume" by tdhooper. https://shadertoy.com/view/wtX3D4[m
[32m+[m[32m// 2019-04-27 01:06:42[m
[32m+[m
[32m+[m
[32m+[m[32mfloat vmax(vec2 v) {[m
[32m+[m[32m    return max(v.x, v.y);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat vmax(vec3 v) {[m
[32m+[m[32m    return max(max(v.x, v.y), v.z);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat vmin(vec3 v) {[m
[32m+[m[32m    return min(min(v.x, v.y), v.z);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat vmin(vec2 v) {[m
[32m+[m[32m    return min(v.x, v.y);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat fBox(vec3 p, vec3 b) {[m
[32m+[m[32m    vec3 d = abs(p) - b;[m
[32m+[m[32m    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mvoid pR(inout vec2 p, float a) {[m
[32m+[m[32m    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat mHead(vec3 p) {[m
[32m+[m[32m    vec3 pa = p;[m
[32m+[m[32m    float bound = fBox(p, vec3(.45,.65,.6));[m
[32m+[m[32m    p.x = -abs(p.x);[m
[32m+[m[32m    p += OFFSET / SCALE;[m
[32m+[m[32m    bound = fBox(p, 1./SCALE);[m
[32m+[m[32m    //return bound;[m
[32m+[m[32m    if (bound > .01) {[m
[32m+[m[32m        return bound;[m
[32m+[m[32m    }[m
[32m+[m[32m    //p.x = -abs(p.x);[m
[32m+[m[32m    //p += OFFSET / SCALE;[m
[32m+[m[32m    p *= SCALE;[m
[32m+[m[32m    float d = mapTex(iChannel0, p);[m
[32m+[m[32m    //return min(d, max(bound, pa.x));[m
[32m+[m[32m    return d;[m
[32m+[m[32m    return min(d, bound + .02);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfloat map(vec3 p) {[m
[32m+[m[32m    // return length(p) - .5;[m
[32m+[m[32m    p.y -= .15;[m
[32m+[m[32m    //pR(p.yz, .2);[m
[32m+[m[32m    // pR(p.xz, iTime/2. + .4);[m
[32m+[m[32m    // pR(p.yz, iTime/2. + .4);[m
[32m+[m[32m    float d = mHead(p);[m
[32m+[m[32m   // d = mix(d, fBox(p, vec3(.7)), sin(iTime) * .5+ .5);[m
[32m+[m[32m    return d;[m
[32m+[m[32m  //  vec2 uv = spaceToTex(p);[m
[32m+[m[32m//    return texture2D(iChannel0, uv).r;[m
[32m+[m[32m    return length(p) - .5;[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mbool isDebug = false;[m
[32m+[m
[32m+[m[32mfloat mapDebug(vec3 p) {[m
[32m+[m[32m    float d = map(p);[m
[32m+[m[32m    return d;[m
[32m+[m[32m    float r = min(abs(p.z), min(abs(p.x), abs(p.y-.05))) - .001;[m
[32m+[m[32m    if (r < d) {[m
[32m+[m[32m        isDebug = true;[m
[32m+[m[32m        return r;[m
[32m+[m[32m    } else {[m
[32m+[m[32m        isDebug = false;[m
[32m+[m[32m    }[m
[32m+[m[32m    return d;[m
[32m+[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mconst int NORMAL_STEPS = 6;[m
[32m+[m[32mvec3 calcNormal(vec3 pos){[m
[32m+[m[32m    vec3 eps = vec3(.0005,0,0);[m
[32m+[m[32m    vec3 nor = vec3(0);[m
[32m+[m[32m    float invert = 1.;[m
[32m+[m[32m    for (int i = 0; i < NORMAL_STEPS; i++){[m
[32m+[m[32m        nor += map(pos + eps * invert) * eps * invert;[m
[32m+[m[32m        eps = eps.zxy;[m
[32m+[m[32m        invert *= -1.;[m
[32m+[m[32m    }[m
[32m+[m[32m    return normalize(nor);[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mvoid mainImage( out vec4 fragColor, in vec2 fragCoord )[m
[32m+[m[32m{[m
[32m+[m[32m    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;[m
[32m+[m[41m    [m
[32m+[m[32m    vec3 camPos = vec3(0,.05,3.2) * .5;[m
[32m+[m[32m    vec3 rayDirection = normalize(vec3(p + vec2(0,-0),-4));[m
[32m+[m[41m    [m
[32m+[m[32m    float r2 = .0;//iTime;[m
[32m+[m[32m    pR(camPos.yz, r2);[m
[32m+[m[32m    pR(rayDirection.yz, r2);[m
[32m+[m
[32m+[m[32m    float r = .5;//iTime + .7;[m
[32m+[m[32m    pR(camPos.xz, r);[m
[32m+[m[32m    pR(rayDirection.xz, r);[m
[32m+[m[41m        [m
[32m+[m[32m    vec3 rayPosition = camPos;[m
[32m+[m[32m    float rayLength = 0.;[m
[32m+[m[32m    float dist = 0.;[m
[32m+[m[32m    bool bg = false;[m
[32m+[m[32m    vec3 col = vec3(0);[m
[32m+[m
[32m+[m[32m    for (int i = 0; i < 300; i++) {[m
[32m+[m[32m        rayLength += dist;[m
[32m+[m[32m        rayPosition = camPos + rayDirection * rayLength;[m
[32m+[m[32m        dist = mapDebug(rayPosition);[m
[32m+[m
[32m+[m[32m        if (abs(dist) < .001) {[m
[32m+[m[32m            break;[m
[32m+[m[32m        }[m
[32m+[m[41m        [m
[32m+[m[32m        if (rayLength > 10.) {[m
[32m+[m[32m            bg = true;[m
[32m+[m[32m            break;[m
[32m+[m[32m        }[m
[32m+[m[32m    }[m
[32m+[m[41m    [m
[32m+[m[32m    if ( ! bg) {[m
[32m+[m[32m        vec3 n = calcNormal(rayPosition);[m
[32m+[m[32m        col = n * .5 + .5;[m
[32m+[m[41m        [m
[32m+[m[32m        if (isDebug) {[m
[32m+[m[32m            float d = map(rayPosition);[m
[32m+[m[32m            col = vec3(mod(d * 10., 1.));[m
[32m+[m[32m        }[m
[32m+[m[32m    }[m
[32m+[m[41m    [m
[32m+[m[32m  //  col = vec3(spaceToTex(vec3(-1,-1,-.7)), 0.);[m
[32m+[m
[32m+[m[32m    //vec2 uv = fragCoord.xy / iResolution.xy;[m
[32m+[m[32m    //vec3 ps = texToSpace(uv)[3].xyz;[m
[32m+[m[32m    //col = abs(ps);[m
[32m+[m[32m    //col = vec3(texture2D(iChannel0, uv).a);[m
[32m+[m[41m    [m
[32m+[m[32m    fragColor = vec4(col,1.0);[m
[32m+[m[32m}[m
\ No newline at end of file[m
