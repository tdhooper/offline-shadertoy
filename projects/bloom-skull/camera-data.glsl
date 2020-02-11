// framebuffer size: 3x1

#pragma glslify: inverse = require(glsl-inverse)
#pragma glslify: import('./quat.glsl')

vec3 calcCylinderNormal(vec3 a, vec3 b, vec3 c) {
    vec3 tan = a - c;
    vec3 bin = cross(a - b, c - b);
    vec3 nor = normalize(cross(tan, bin));
    return nor;
}

vec3 calcAxis() {
    vec3 up = vec3(0,-1,0);

    // calculate first four points, ignoring scaling
    // these form a cylinder
    vec3 v0, v1, v2, v3;
    vec4 r;
    v0 = vec3(0);
    v1 = stepPosition;
    r = q_look_at(stepNormal, up);
    v2 = v1 + rotate_vector(stepPosition, r);
    r = q_look_at(rotate_vector(stepNormal, r), rotate_vector(up, r));
    v3 = v2 + rotate_vector(stepPosition, r);

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
    vec3 up = vec3(0,-1,0);
    vec3 nor = axis;
    vec3 bin = normalize(cross(nor, up));
    vec3 tan = normalize(cross(nor, bin));
    mat3 mAxis = mat3(nor, bin, tan);
    return mAxis;
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
    vec3 up = vec3(0,-1,0);

    // calculate first three points, ignoring scaling
    // these form a circle
    vec3 v0, v1, v2;
    vec4 r;
    v0 = vec3(0);
    v1 = stepPosition;
    r = q_look_at(stepNormal, up);
    v2 = v1 + rotate_vector(stepPosition, r);

    // project points onto axis plane
    vec2 point0 = (v0 * mAxis).yz;
    vec2 point1 = (v1 * mAxis).yz;
    vec2 point2 = (v2 * mAxis).yz;

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

    // calculate first two points, with scaling
    // these are the logarithmic points
    vec3 v0s, v1s;
    float s;
    s = 1. / stepScale;
    v0s = vec3(0);
    s *= stepScale;
    v1s = stepPosition * s;

    // project points onto axis plane
    vec2 point0s = (v0s * mAxis).yz;
    vec2 point1s = (v1s * mAxis).yz;

    // calculate the center of the logarithmic spiral
    vec2 center2 = calcCenter(point0s, point1s, stepScale, spokeAngle);

    // transform back into 3d
    vec3 center = vec3(0, center2) * inverse(mAxis);


    float v1Height = dot(v1s, axis);
    // project onto cylinder base at origin
    vec3 v1p = v1s - axis * v1Height;
    float v1Radius = length(v1p);

    vec3 apex = center + axis * v1Height * (length(v1p) / length(center));

    
    // // height of cone
    // a2 = b2+c2

    // float height = sqrt(v1Height * v1Height + v1Radius * v1Radius);
    // center -= dot(center, axis) * axis;
    return center;
    // return apex;
}

vec3 findCenter() {
    vec3 up = vec3(0,-1,0);

    float s = 1. / stepScale;
    vec3 v = vec3(0);
    vec4 r = QUATERNION_IDENTITY;

    for (int i = 0; i < 2; i++) {
        s *= stepScale;
        v += rotate_vector(stepPosition * s, r);
        r = q_look_at(rotate_vector(stepNormal, r), rotate_vector(up, r));
    }

    return v;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec3 axis = calcAxis();
    mat3 mAxis = calcAxisMatrix(axis);
    float spokeAngle = calcSpokeAngle(mAxis);
    // vec3 center = calcCenter(axis, mAxis, spokeAngle);
    vec3 center = findCenter();
    if (fragCoord.x < 1.) {
        fragColor = vec4(center, 1);
    } else if (fragCoord.x < 2.) {
        fragColor = vec4(axis, 1);
    } else {
        fragColor = vec4(spokeAngle, 0, 0, 1);
    }
}
