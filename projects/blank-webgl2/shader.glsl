#version 300 es

precision highp float;

out vec4 fragColor;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColor, gl_FragCoord.xy);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  fragColor = vec4(1,0,0,1);
}
