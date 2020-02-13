uniform sampler2D iChannel0; // camera-data.glsl filter: nearest wrap: clamp
uniform vec2 iChannel0Size;


vec3 wayOrigin;
vec3 wayAxis;
float wayAngle;


#pragma glslify: import('./camera-precalc.glsl')


void calcWaypoints() {
    wayAxis = calcAxis();
    mat3 mAxis = calcAxisMatrix(wayAxis);
    wayAngle = calcSpokeAngle(mAxis);
    wayOrigin = calcCenter(wayAxis, mAxis, wayAngle);
    // wayOrigin = findCenter();
}

float tweenCamera(inout vec3 p, float t) {
    float scale = pow(stepScale, t);
    float angle = abs(wayAngle) * t;
    vec4 rot = rotate_angle_axis(angle, wayAxis);
    p -= wayOrigin;
    p = rotate_vector(p, rot);
    p *= scale;    
    p += wayOrigin;
    return scale;
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fLine(vec3 p, vec3 n) {
    float t = dot(p, n) / dot(n, n);
    return length((n * t) - p) ;
}

float fWaypointB(vec3 p) {
    float s = 1.;
    float d = fBox(p, vec3(.02, .05, .03) * s);
    // d = min(d, max(length(p.yz) - .001, -p.x));
    // d = min(d, fLine(p, stepNormal) - .001);
    // d = min(d, abs(p.z) - .001);
    p -= vec3(.01,.025,.04) * s;
    d = min(d, fBox(p, vec3(.01,.025,.01) * s));
    return d;
}

float mapWaypoints(vec3 p) {
    float path = 1e12;
    vec3 pp = p;
    float scale;
    const float WITER = 6.;
    for (float i = 0.; i < WITER; i++){
        p = pp;
        scale = tweenCamera(p, -i);
        //path = min(path, length(p - tweenCameraPos(i / WITER)) - .005);
        path = min(path, fWaypointB(p) / scale);
    }
    p = pp;


    float d = path;

    float axis = fLine(p - wayOrigin, wayAxis) - .002;

    d = min(d, axis);
    d = min(d, length(p - wayOrigin) - .02);

    return d;
}
