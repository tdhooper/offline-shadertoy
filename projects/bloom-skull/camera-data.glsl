// framebuffer size: 2x1

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

    vec3 v0, v1, v2, v3;
    vec4 r0, r1, r2, r3;

    v0 = vec3(0);
    r0 = QUATERNION_IDENTITY;

    v1 = stepPosition;
    r1 = q_look_at(stepNormal, up);

    v2 = v1 + rotate_vector(stepPosition, r1);
    r2 = q_look_at(rotate_vector(stepNormal, r1), rotate_vector(up, r1));

    v3 = v2 + rotate_vector(stepPosition, r2);
    r3 = q_look_at(rotate_vector(stepNormal, r2), rotate_vector(up, r2));


    vec3 n0 = calcCylinderNormal(v0, v1, v2);
    vec3 n1 = calcCylinderNormal(v1, v2, v3);

    // rotation matrix for cylinder direction
    vec3 nor = normalize(cross(n0, n1));

    return nor;
}

vec3 calcCenter() {
    vec3 up = vec3(0,-1,0);

    float s = 1. / stepScale;
    vec3 v = vec3(0);
    vec4 r = QUATERNION_IDENTITY;

    for (int i = 0; i < 100; i++) {
        s *= stepScale;
        v += rotate_vector(stepPosition * s, r);
        r = q_look_at(rotate_vector(stepNormal, r), rotate_vector(up, r));
    }

    return v;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    if (fragCoord.x > 1.) {
        vec3 axis = calcAxis();
        fragColor = vec4(axis, 1);
    } else {
        vec3 center = calcCenter();
        fragColor = vec4(center, 1);
    }
}
