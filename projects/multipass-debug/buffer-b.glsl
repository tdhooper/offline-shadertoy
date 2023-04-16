// framebuffer drawcount: 5

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform sampler2D previousSample; // buffer-b.glsl filter: linear
uniform sampler2D dataBuffer; // buffer-a.glsl filter: nearest
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

// https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;

    vec4 col = vec4(vec3(hash22((uv + float(iFrame)) * 1000.).x), 1);

    if (uv.x < .5 && uv.y < .5) {
        col = mix(col, texture2D(dataBuffer, uv / .5), .5);
    }

    if (drawIndex > 0.) {
        vec4 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }

    fragColor = col;
}
