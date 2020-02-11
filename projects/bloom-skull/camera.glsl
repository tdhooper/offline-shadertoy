uniform sampler2D iChannel0; // camera-data.glsl filter: nearest wrap: clamp
uniform vec2 iChannel0Size;

struct Waypoint {
    vec3 trans;
    vec4 rot;
    float scale;
};

Waypoint way0;
Waypoint way1;
Waypoint way2;
Waypoint way3;

vec3 wayOrigin;
vec3 wayAxis;
float wayAngle;

void calcWaypoints() {
    wayOrigin = texture2D(iChannel0, vec2(0,0)).rgb;
    wayAxis = texture2D(iChannel0, vec2(.5,0)).rgb;
    wayAngle = texture2D(iChannel0, vec2(1,0)).r;

    vec3 up = vec3(0,-1,0);
    vec3 normal = stepNormal;

    way0.scale = 1. / stepScale;
    way0.trans = vec3(0);
    way0.rot = QUATERNION_IDENTITY;

    way1.scale = way0.scale * stepScale;
    way1.trans = stepPosition * way1.scale;
    way1.rot = q_look_at(normal, up);

    way2.scale = way1.scale * stepScale;
    way2.trans = way1.trans + rotate_vector(stepPosition * way2.scale, way1.rot);
    way2.rot = q_look_at(rotate_vector(normal, way1.rot), rotate_vector(up, way1.rot));

    way3.scale = way2.scale * stepScale;
    way3.trans = way2.trans + rotate_vector(stepPosition * way3.scale, way2.rot);
    way3.rot = q_look_at(rotate_vector(normal, way2.rot), rotate_vector(up, way2.rot));
}


vec3 Catmull(vec3 p0, vec3 p1, vec3 p2, vec3 p3, float t){
    return (((-p0 + p1*3. - p2*3. + p3)*t*t*t + (p0*2. - p1*5. + p2*4. - p3)*t*t + (-p0 + p2)*t + p1*2.)*.5);
}

vec3 bezier(vec3 p0, vec3 p1, vec3 p2, vec3 p3, float t) {
    vec3 a0 = mix(p0, p1, t);
    vec3 a1 = mix(p1, p2, t);
    vec3 a2 = mix(p2, p3, t);
    vec3 b0 = mix(a0, a1, t);
    vec3 b1 = mix(a1, a2, t);
    return mix(a0, b1, t);
}

vec3 midpointNormal(vec3 a, vec3 b, vec3 c) {
    vec3 an = normalize(a - b);
    vec3 cn = normalize(c - b);
    vec3 mid = mix(an, cn, .5);
    return normalize(-mid);
}

vec3 planeNormal(vec3 a, vec3 b, vec3 c) {
    return normalize(cross(b - a, c - a));
}

vec3 controlDir(vec3 a, vec3 b, vec3 c) {
    return cross(
        midpointNormal(a, b, c),
        planeNormal(a, b, c)
    );
}

vec3 tweenCameraPos(float t) {
    vec3 c1 = way1.trans - controlDir(way0.trans, way1.trans, way2.trans) * distance(way1.trans, way2.trans) * .6;
    vec3 c2 = way2.trans + controlDir(way1.trans, way2.trans, way3.trans) * distance(way1.trans, way2.trans) * .2;
    t = pow(t, 1.08); // correct for non-constant speed
    return bezier(way1.trans, c1, c2, way2.trans, t);
}

float tweenCamera(inout vec3 p, float t) {
    //float ramp = (pow(stepScale, t) - 1.) / (stepScale - 1.);

    //t = -t;
    //t *= 2.;

    t = mix(-2., 2., t);

    float scale = pow(stepScale, t);
    //float scale = mix(1., stepScale, ramp);

    float angle = abs(wayAngle) * t;
    vec4 rot = rotate_angle_axis(angle, wayAxis);

    vec3 o = wayOrigin + wayAxis;

    p -= o;
    p = rotate_vector(p, rot);
    // p += wayAxis * pow(t, stepScale) * sign(t) * dot(stepPosition, wayAxis);
    p *= scale;
    p += o;
    return scale;

    // vec3 up = vec3(0,-1,0);
    // vec4 rot = q_slerp(way1.rot, way2.rot, t);
    // vec3 trans = tweenCameraPos(ramp);
    // p *= scale;
    // p = rotate_vector(p, rot);
    // p += trans;
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
    float s = 2.;
    float d = fBox(p, vec3(.02, .05, .03) * s);
    // d = min(d, max(length(p.yz) - .001, -p.x));
    // d = min(d, fLine(p, stepNormal) - .001);
    // d = min(d, abs(p.z) - .001);
    p -= vec3(.01,.025,.04) * s;
    d = min(d, fBox(p, vec3(.01,.025,.01) * s));
    return d;
}

float fWaypoint(vec3 p, Waypoint w) {
    float s = 2.;
    p -= w.trans;
    p = rotate_vector(p, q_conj(w.rot));
    float d = fWaypointB(p / w.scale) * w.scale;
    return d;
}


float mapWaypoints(vec3 p) {
    float path = 1e12;
    vec3 pp = p;
    float scale;
    const float WITER = 20.;
    for (float i = 0.; i < WITER; i++){
        p = pp;
        scale = tweenCamera(p, i / (WITER - 1.));
        p *= stepScale;
        scale *= stepScale;
        //path = min(path, length(p - tweenCameraPos(i / WITER)) - .005);
        path = min(path, fWaypointB(p) / scale);
    }
    p = pp;

    float blocks = min(
        min(
            fWaypoint(p, way0),
            fWaypoint(p, way1)
        ),
        min(
            fWaypoint(p, way2),
            fWaypoint(p, way3)
        )
    );

    // float axis = fLine(p, wayAxis) - .02;
    
    // float axis = fLine(p - wayOrigin, wayAxis) - .005;
    // axis = max(axis, length(p - wayOrigin) - 1.);

    float axis = fLine(p - wayOrigin, wayAxis) - .005;
    // axis = max(axis, length(p - wayOrigin) - 1.);

    float d = min(path, blocks);
    d = min(d, axis);

    // d = min(d, length(p - debug0) - .03);
    // d = min(d, length(p - debug1) - .03);

    // float pl = abs(dot(p, wayAxis)) - .01;
    // pl = max(pl, length(p - wayOrigin) - .5);
    // d = min(d, pl);

    // d = min(d, fLine(p - debug0, wayAxis) - .005);
    // d = min(d, fLine(p - debug1, wayAxis) - .005);

    

    return d;
}
