#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform int drawIndex;

out vec4 fragColorOut;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColorOut, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

#pragma glslify: blur = require('./blur13', texture=texture)

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  if (drawIndex == 0) {
    fragColor = blur(iChannel0, fragCoord.xy / iResolution.xy, iResolution.xy, vec2(1,0));
  } else {
    fragColor = texture(iChannel0, fragCoord.xy / iResolution.xy);
  }}