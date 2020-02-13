

vec3 calcCylinderNormal(vec3 a, vec3 b, vec3 c) {
    vec3 tan = a - c;
    vec3 bin = cross(a - b, c - b);
    vec3 nor = normalize(cross(tan, bin));
    return nor;
}

vec3 calcAxis() {
    // calculate first four points, ignoring scaling
    // these form a cylinder
    vec3 v0 = vec3(0);
    vec3 v1 = stepPosition;
    vec3 v2 = v1 + stepRotate * stepPosition;
    vec3 v3 = v2 + stepRotate * stepRotate * stepPosition;

    // calculate normals for the two middle points
    // based on samples from each side
    vec3 n0 = calcCylinderNormal(v0, v1, v2);
    vec3 n1 = calcCylinderNormal(v1, v2, v3);

    // get the cylinder axis
    vec3 axis = normalize(cross(n0, n1));

    return axis;
}

// rotation matrix for cylinder direction
mat3 calcAxisMatrix(vec3 axis) {
    return basisMatrix(axis, vec3(0,1,0));
}

// find angle between ab and ac
float findAngle(vec2 a, vec2 b, vec2 c) {
    return acos(dot(normalize(b - a), normalize(c - a)));
}

// calculate signed angle between each spoke of the spiral
// we can do this by ignoring the scaling factor
float calcSpokeAngle(vec2 a, vec2 b, vec2 c) {
    float angle = findAngle(b, a, c);
    angle = PI - angle;
    // are we angled to the left or right?
    float side = sign((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x));
    return angle * -side;
}

float calcSpokeAngle(mat3 mAxis) {
    // calculate first three points, ignoring scaling
    // these form a circle when projected onto the axis
    vec3 v0 = vec3(0);
    vec3 v1 = stepPosition;
    vec3 v2 = v1 + stepRotate * stepPosition;

    // project points onto axis plane
    vec2 point0 = (v0 * mAxis).xy;
    vec2 point1 = (v1 * mAxis).xy;
    vec2 point2 = (v2 * mAxis).xy;

    // calculate angle between each spoke of the circle
    // this is the same for scaled and unscaled points, but it's easier
    // to calculate for unscaled
    float spokeAngle = calcSpokeAngle(point0, point1, point2);

    return spokeAngle;
}

vec2 rotate(vec2 p, float a) {
    return p * mat2(cos(a), sin(a), -sin(a), cos(a));
}

vec2 calcCenter(vec2 point0, vec2 point1, float scale, float spokeAngle) {

    // scaling factor for each iteration
    float s = scale;

    // distance between first two points
    float side0 = distance(point0, point1);

    // angle between each spoke
    float angle0 = spokeAngle;
    
    // length of side from first point to spiral center
    
    // using cosine rule:
    // https://en.wikipedia.org/wiki/Solution_of_triangles#Two_sides_and_the_included_angle_given_(SAS)
    // c = sqrt(a^2 + b^2 - 2abcos(γ))
    
    // when b = a * s:
    // c = sqrt(a^2 + (as)^2 - 2a(as)cos(Y))
    
    // solve for a:
    // https://www.wolframalpha.com/widgets/view.jsp?id=c778a2d8bf30ef1d3c2d6bc5696defad
    // a = c / sqrt(s^2 - 2 s cos(γ) + 1)

    float side1 = side0 / sqrt((s * s) - 2. * s * cos(angle0) + 1.);
    
    // b = a * s
    float side2 = s * side1;
    
    // opposite angle to side2, using sine law
    // https://en.wikipedia.org/wiki/Law_of_sines#Example_1
    //float angle1 = asin((side1 * sin(angle0)) / side0);
    float angle2 = asin((side2 * sin(angle0)) / side0);

    // find the center from the angle and side length
    vec2 center = vec2(sin(angle2), cos(angle2)) * side1;
 
    // rotate and translate into position
    vec2 v = point1 - point0;
    center = rotate(center, atan(v.x, v.y));
    center += point0;
    
    return center;
}

vec3 calcCenter(vec3 axis, mat3 mAxis, float spokeAngle) {

    // calculate first two points
    vec3 v0 = vec3(0);
    vec3 v1 = stepPosition;

    // project points onto axis plane
    vec2 point0 = (v0 * mAxis).xy;
    vec2 point1 = (v1 * mAxis).xy;

    // calculate the center of the logarithmic spiral
    vec2 center2 = calcCenter(point0, point1, stepScale, spokeAngle);

    // transform back into 3d
    vec3 center = vec3(center2, 0) * inverse(mAxis);

    // extrapolate the line between v0 and v1 to find the apex
    float v1Height = dot(v1, axis);
    float v1Radius = distance(center, v1 - axis * v1Height);
    vec3 apex = center + axis * v1Height * (length(center) / (length(center) - v1Radius));

    return apex;
}

vec3 findCenter() {
    float s = 1. / stepScale;
    vec3 v = vec3(0);
    mat3 m = mat3(
        1,0,0,
        0,1,0,
        0,0,1
    );

    for (int i = 0; i < 100; i++) {
        s *= stepScale;
        v += m * stepPosition * s;
        m *= stepRotate;
    }

    return v;
}

