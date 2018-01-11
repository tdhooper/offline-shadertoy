
// --------------------------------------------------------
// GEOMETRY TOOLS
// --------------------------------------------------------

float smax(float a, float b, float r) {
    float m = max(a, b);
    if ((-a < r) && (-b < r)) {
        return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
    } else {
        return m;
    }
}

// The "Round" variant uses a quarter-circle to join the two objects smoothly:
float smin(float a, float b, float r) {
    float m = min(a, b);
    if ((a < r) && (b < r) ) {
        return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
    } else {
     return m;
    }
}

float fCone(vec3 p, float radius, float height, vec3 direction, float offset) {
    p -= direction * offset;
    p = reflect(p, normalize(mix(vec3(0,1,0), -direction, .5)));
    //p -= vec3(0,height,0);
    return fCone(p, radius, height);
}

float fCone(vec3 p, float radius, float height, vec3 direction) {
    return fCone(p, radius, height, direction, 0.);
}

float fConeI(vec3 p, float radius, float height, vec3 direction) {
    return fCone(p, radius, height, -direction, -height);
}

float fConeI(vec3 p, float radius, float height, vec3 direction, float offset) {
    return fCone(p, radius, height, -direction, -height - offset);
}

float fCone(vec3 p, float radius, vec3 start, vec3 end) {
    float height = length(start - end);
    vec3 direction = normalize(end - start);
    return fCone(p - start, radius, height, direction);
}




float fPolyR(vec3 p, float thickness, vec3 n1, vec3 n2) {
    return fPlane(p, normalize(cross(n1, n2)), thickness);
}
    
float fPolyR(vec3 p, float thickness, float round, float d, vec3 n1, vec3 n2) {
    return smax(d, fPolyR(p, thickness, n1, n2), round);
}

float fPoly(vec3 p, float thickness, float round, vec3 n1, vec3 n2, vec3 n3) {
    float d = fPolyR(p, thickness, n1, n2);
    d = fPolyR(p, thickness, round, d, n2, n3);
    d = fPolyR(p, thickness, round, d, n3, n1);
    return d;
}

float fPoly(vec3 p, float thickness, float round, vec3 n1, vec3 n2, vec3 n3, vec3 n4) {
    float d = fPolyR(p, thickness, n1, n2);
    d = fPolyR(p, thickness, round, d, n2, n3);
    d = fPolyR(p, thickness, round, d, n3, n4);
    d = fPolyR(p, thickness, round, d, n4, n1);
    return d;
}

float fPoly(vec3 p, float thickness, float round, vec3 n1, vec3 n2, vec3 n3, vec3 n4, vec3 n5) {
    float d = fPolyR(p, thickness, n1, n2);
    d = fPolyR(p, thickness, round, d, n2, n3);
    d = fPolyR(p, thickness, round, d, n3, n4);
    d = fPolyR(p, thickness, round, d, n4, n5);
    d = fPolyR(p, thickness, round, d, n5, n1);
    return d;
}

float fPoly(vec3 p, float thickness, float round, vec3 n1, vec3 n2, vec3 n3, vec3 n4, vec3 n5, vec3 n6) {
    float d = fPolyR(p, thickness, n1, n2);
    d = fPolyR(p, thickness, round, d, n2, n3);
    d = fPolyR(p, thickness, round, d, n3, n4);
    d = fPolyR(p, thickness, round, d, n4, n5);
    d = fPolyR(p, thickness, round, d, n5, n6);
    d = fPolyR(p, thickness, round, d, n6, n1);
    return d;
}


vec3 projectOnPlane(vec3 v, vec3 n) {
    float scalar = dot(n, v) / length(n);
    vec3 v1 = n * scalar;
    return v - v1;
}

// Scalar representing the closest normal to p
// on the spherical chord between two normals
float scalarBetween(vec3 p, vec3 n1, vec3 n2) {
    vec3 plane = normalize(cross(n1, n2));
    vec3 projected = normalize(projectOnPlane(p, plane));
    float scalar = (acos(dot(projected, n1))) / (acos(dot(n1, n2)));
    //float scalar = (1. - dot(projected, n1)) / (1. - dot(n1, n2));
    scalar = clamp(scalar, 0., 1.);
    return scalar;
}

float fSpike42(vec3 p, vec3 c1, vec3 c2) {
    return fCone(p, .08, c1, c2) - .02;
}




float fCrater(vec3 p, float d, vec3 n) {
    float outer = length(p) - 1.;
    float spike = fCone(p, .6, n * 0., n * 2.);
    d = smin(d, spike, .15);

    float shell = smin(d, outer, .15) + .1;    
    d = max(d, -shell);
    
    float hole = fCone(p, .3, n * 2., n * -1.);
    d = smax(d, -hole, .05);
    return d;
}
