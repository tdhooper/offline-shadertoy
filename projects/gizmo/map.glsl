uniform mat4 gizmoAdjustmentMatrix;

vec3 GIZMO_LOCAL_P;
vec3 GIZMO_LOCAL_P2;

void GIZMO(inout vec3 p, mat4 m) {
    GIZMO_LOCAL_P = p;
    p = (m * vec4(p, 1)).xyz;
    GIZMO_LOCAL_P2 = p;
    p = (gizmoAdjustmentMatrix * vec4(p, 1)).xyz;
}

void GIZMO(inout vec3 p) {
    GIZMO_LOCAL_P = p;
    GIZMO_LOCAL_P2 = p;
    p = (gizmoAdjustmentMatrix * vec4(p, 1)).xyz;
}

#define saturate(x) clamp(x, 0., 1.)

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin3(float a, float b, float k){
    return min(
        smin(a, b, k),
        smin2(a, b, k)
    );
}

float smax3(float a, float b, float k){
    return max(
        smax(a, b, k),
        smax2(a, b, k)
    );
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Shortcut for 45-degrees rotation
void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

vec3 pRx(vec3 p, float a) {
    pR(p.yz, a); return p;
}

vec3 pRy(vec3 p, float a) {
    pR(p.xz, a); return p;
}

vec3 pRz(vec3 p, float a) {
    pR(p.xy, a); return p;
}



float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmax2(vec2 v) {
    return max(v.x, v.y);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}


float map(vec3 p) {
    
    float d = 1e12;

    d = min(d, fBox(p, vec3(.5)));

    p -= vec3(1, .5, -.25);
    pR(p.xy, -.2);
    pR(p.xz, .8);
    pR(p.yz, .4);

    float scl = .5;
    p /= scl;

GIZMO(p, mat4(1.0000420808792114,0.00007718510460108519,-0.000029137088858988136,0,0.00003767923408304341,1.0000700950622559,-0.000026215095203951932,0,-0.000011497474588395562,-0.000020516934455372393,1.0000078678131104,0,0.050561077892780304,-0.17443862557411194,0.05476408824324608,1));

    d = smin(d, fBox(p, vec3(.5)) * scl, .5);

    return d;
}

#pragma glslify: export(map)
