precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */


// Maximum/minumum elements of a vector
float vmax(vec2 v) {
    return max(v.x, v.y);
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

float fBoxy(vec2 p, vec2 s) {
    return vmax(abs(p) - s);
}


float fNova(vec2 p) {

    float d = 1e12;
    vec2 p2 = p;
    vec2 p3 = p;
    float arrow;

    // N
    p.y += .05;
    p3 = p;
    d = min(d, fBoxy(p, vec2(.6, .45)));
    pR45(p);
    d = max(d, fBoxy(p, vec2(1., .55)));
    p = p3;
    p.x += .03;
    p.x *= sign(p.y);
    p.y = abs(p.y);
    d = max(d, -fBoxy(p - vec2(.166, .5), vec2(.03,.45)));
    p = p2;

    // O
    p.x -= 1.;
    p2 = p;
    float O = fBoxy(p, vec2(.5,.5));
    pR45(p);
    O = max(O, abs(p.y) - .55);
    O = max(O, -d + .06);
    d = min(d, O);
    p = p2;

    p += vec2(.25,-.15);
    p3 = p;
    p.y *= 1.25;
    pR45(p);
    arrow = vmax(p);
    p = p3;
    arrow = max(arrow, fBoxy(p, vec2(.21, 1.)));
    d = max(d, -arrow + .06);
    d = min(d, arrow);
    p = p2;
    d = min(d, fBoxy(p - vec2(0,-.4), vec2(.1,.2)));
    d = max(d, -p.y - .5);

    // V
    p.x -= .8;
    p2 = p;
    p.y += .95;
    p.x = abs(p.x);
    pR(p, -.4);
    float V = p.x;
    p = p2;
    V = max(V, abs(p.y) - .5);
    p.y -= .6;
    V = max(V, -fBoxy(p, vec2(.03, .5)));
    p = p2;
    d = max(d, -V + .06);
    p.y -= .4;
    V = max(V, -vmax(p * vec2(1,-1)));
    d = min(d, V);
    p = p2;

    // A
    p.x -= .75;
    p2 = p;
    p.y += .1;
    float A = fBoxy(p, vec2(.45, .4));
    p = p2;
    pR45(p);
    A = max(A, -p.x - .6);
    A = max(A, -V + .06);
    d = min(d, A);
    p = p2;

    p -= vec2(-.37, .2);
    arrow = abs(p.y) - .15;
    pR(p, .5);
    arrow = max(arrow, -p.x);
    d = max(d, -arrow +.06);
    d = min(d, arrow);
    p = p2;
    p.x -= .25;
    d = min(d, fBoxy(p, vec2(.3)));
    p = p2;
    d = max(d, p.x- .45);
    pR45(p);
    d = max(d, p.x - .43);

    p = p2;
    d = max(d, -fBoxy(p + vec2(0,.23), vec2(.1, .03)));

    return d;
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    p.x += 1.;
    p *= 1.5;
    vec3 color;

    float d = fNova(p);

    color = vec3(smoothstep(0.01, .0, d));
    color += vec3(0,1,1) * mod(d * 5., 1.) * .5;

    fragColor = vec4(color,1.0);
}
