// framebuffer size: 3x1

#pragma glslify: inverse = require(glsl-inverse)
#pragma glslify: import('./quat.glsl')
#pragma glslify: import('./camera-precalc.glsl')

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
