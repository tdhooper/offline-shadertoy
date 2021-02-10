// framebuffer size: 1300x1300

precision highp float;

uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform vec2 iChannel0Size;
uniform bool firstPass;

#pragma glslify: sdSkull = require(../skull/skull.glsl)

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float map(vec3 p) {
    p -= OFFSET;
    p /= SCALE;
    pR(p.xz, iTime);
    return sdSkull(p);
    // return fBox(p, vec3(.5));
    // return length(p) - .4;
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 coord = fragCoord.xy;
    vec2 size = iResolution;
    vec2 uv = coord / size;
    
    if ( ! firstPass) {
        //fragColor = texture2D(iChannel0, uv);
       // return;
    }
    
    vec3 p0 = texToSpace(coord, size)[0].xyz;
    vec3 p1 = texToSpace(coord, size)[1].xyz;
    vec3 p2 = texToSpace(coord, size)[2].xyz;
    vec3 p3 = texToSpace(coord, size)[3].xyz;

    fragColor = vec4(
        map(p0),
        map(p1),
        map(p2),
        map(p3)
    );
}