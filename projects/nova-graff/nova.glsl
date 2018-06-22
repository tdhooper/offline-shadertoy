
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


float fNova(vec2 p) {

    float d = 1e12;
    vec2 p2 = p;
    vec2 p3 = p;
    float arrow;

    float weight = .1;

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
    d = max(d, -fBoxy(p - vec2(.166, .5), vec2(weight/2.,.45)));
    p = p2;

    // O
    p.x -= 1.;
    p2 = p;
    float O = fBoxy(p, vec2(.5,.5));
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
    arrow = max(arrow, fBoxy(p, vec2(.21, 1.)));
    d = max(d, -arrow + weight);
    d = min(d, arrow);
    p = p2;
    d = min(d, fBoxy(p - vec2(0,-.4), vec2(.1,.2)));
    d = max(d, -p.y - .5);

    // V
    p.x -= .8;
    p2 = p;
    p.y += .95;
    p.x = abs(p.x);
    pR(p, -.42);
    float V = p.x;
    p = p2;
    V = max(V, abs(p.y) - .5);
    p.y -= .6;
    V = max(V, -fBoxy(p, vec2(weight/2., .5)));
    p = p2;
    p.y -= .35;
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
    pR45(p);
    A = max(A, -p.x - .6);
    A = max(A, -V + weight);
    d = min(d, A);
    p = p2;

    p -= vec2(-.4, .2);
    arrow = abs(p.y) - .125;
    p.y -= .05;
    pR45(p);
    arrow = max(arrow, vmax(p * vec2(-1,1)));
    d = max(d, -arrow + weight);
    d = min(d, arrow);
    p = p2;
    p.x -= .3;
    d = min(d, fBoxy(p, vec2(.15)));
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
