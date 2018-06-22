
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

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBoxy(vec2 p, vec2 s) {
    return vmax(abs(p) - s);
}

// The "Round" variant uses a quarter-circle to join the two objects smoothly:
float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin(float a, float b) {
    return smin(a, b, 0.);
}

float smax(float a, float b) {
    return smax(a, b, 0.);
}

float svmax(vec2 v) {
    return smax(v.x, v.y);
}


float fNova(vec2 p, float weight) {

    float d = 1e12;
    vec2 p2 = p;
    vec2 p3 = p;
    float arrow;

    // N
    // p.y -= .05;
    p3 = p;
    d = min(d, fBoxy(p, vec2(.6, .5)));
    p.x -= .1;
    pR45(p);
    d = max(d, fBoxy(p, vec2(1., .6)));
    p = p3;
    p.x += .03;
    // p.x *= sign(p.y);
    // p.y = abs(p.y);
    d = max(d, -fBoxy(p - vec2(.2, .5), vec2(weight/2.,.6)));
    d = max(d, -fBoxy(p + vec2(.2, .5), vec2(weight/2.,.6)));
    p = p2;

    // O
    p.x -= .9;
    p2 = p;
    float O = fBoxy(p, vec2(.5,.6));
    pR45(p);
    O = max(O, vmax(abs(p)) - .55);
    O = max(O, -d + weight);
    d = min(d, O);
    p = p2;

    p += vec2(.25,-.2);
    p3 = p;
    p.y *= 1.25;
    pR45(p);
    arrow = vmax(p);
    p = p3;
    arrow = max(arrow, fBoxy(p, vec2(.1, 1.)));
    d = max(d, -arrow + weight);
    d = min(d, arrow);
    p = p2;
    d = min(d, fBoxy(p - vec2(-.1,-.4), vec2(.25,.15)));
    d = max(d, -p.y - .5);

    // V
    p.x -= .8;
    p2 = p;
    p.y += .95;
    p.x = abs(p.x);
    pR(p, -.42);
    float V = p.x;
    p = p2;
    V = max(V, abs(p.y) - .6);
    p.y -= .6;
    p.x -= .025;
    V = max(V, -fBoxy(p, vec2(weight/2., .75)));
    p = p2;
    p.x -= .075;
    p.y -= .3;
    V = max(V, -vmax(p * vec2(1,-1)));
    d = max(d, -V + weight);
    d = min(d, V);
    p = p2;

    // A
    p.x -= .75;
    p2 = p;
    p.y += .1;
    float A = fBoxy(p, vec2(.45, .4));
    p = p2;
    A = max(A, -V + weight);
    d = min(d, A);
    p = p2;

    p -= vec2(-.3, .2);
    arrow = abs(p.y) - .125;
    p.y -= .05;
    pR45(p);
    pR(p, -.3);
    arrow = max(arrow, -p.x * 1.5);
    d = max(d, -arrow + weight);
    d = min(d, arrow);
    p = p2;
    p.x -= .3;
    d = min(d, fBoxy(p, vec2(.15, .3)));
    p = p2;
    d = max(d, p.x- .45);
    pR45(p);
    d = max(d, p.x - .43);

    p = p2;
    d = max(d, -fBoxy(p + vec2(-.05,.23), vec2(.1, weight/2.)));

    // d = arrow;

    return d;
}

#pragma glslify: export(fNova)
