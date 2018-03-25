precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;

uniform float guiRotation;
uniform float guiScale;
uniform float guiOffsetX;
uniform float guiOffsetY;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

// --------------------------------------------------------
// https://github.com/stackgl/glsl-inverse
// --------------------------------------------------------

mat3 inverse(mat3 m) {
  float a00 = m[0][0], a01 = m[0][1], a02 = m[0][2];
  float a10 = m[1][0], a11 = m[1][1], a12 = m[1][2];
  float a20 = m[2][0], a21 = m[2][1], a22 = m[2][2];

  float b01 = a22 * a11 - a12 * a21;
  float b11 = -a22 * a10 + a12 * a20;
  float b21 = a21 * a10 - a11 * a20;

  float det = a00 * b01 + a01 * b11 + a02 * b21;

  return mat3(b01, (-a22 * a01 + a02 * a21), (a12 * a01 - a02 * a11),
              b11, (a22 * a00 - a02 * a20), (-a12 * a00 + a02 * a10),
              b21, (-a21 * a00 + a01 * a20), (a11 * a00 - a01 * a10)) / det;
}


const float sqrt3 = 1.7320508075688772;
const float i3 = 0.5773502691896258;

const mat2 cart2hex = mat2(1, 0, i3, 2. * i3);
const mat2 hex2cart = mat2(1, 0, -.5, .5 * sqrt3);

#define PI 3.14159265359
#define PHI (1.618033988749895)
#define TAU 6.283185307179586

const float c6 = cos(TAU/6.);
const float s6 = sin(TAU/6.);
const mat2 rot6 = mat2(c6, -s6, s6, c6);

struct Vec23 {
    vec2 a;
    vec2 b;
    vec2 c;
};


// --------------------------------------------------------
// Adapted from mattz
// https://www.shadertoy.com/view/4d2GzV
// --------------------------------------------------------

Vec23 shapePoints(vec2 p) {

    vec2 pTri = cart2hex * p;
    vec2 pi = floor(pTri);
    vec2 pf = fract(pTri);

    // Tri
    
    vec2 vv = vec2(1./3.,2./3.);
    vec2 tri = mix(vv, vv.yx, step(pf.y, pf.x));

    tri += pi;
    tri = hex2cart * tri;
    
    // Hex
    
    float split0 = step(pf.x * .5, pf.y);
    float split1 = step(pf.x * .5 + .5, pf.y);
    float split2 = step(pf.x, 1. - pf.y);
    float split3 = step(pf.y * .5 + (.5 * split2), pf.x);
    
    vec2 left = vec2(0, split1);
    vec2 right = vec2(1, split0);
    vec2 hex = mix(left, right, split3);
            
    hex += vec2(pi);
    hex = hex2cart * hex;

    // Next nearest tri

    vec2 nearA = rot6 * (tri - hex) + hex;
    vec2 nearB = (tri - hex) * rot6 + hex;
    
    if (length(nearB - p) > length(nearA - p)) {
        nearA = nearA;
    } else {
        nearA = nearB;
    }
    
    return Vec23(tri, nearA, hex);
}

mat3 rotate2d(float _angle){
    return mat3(cos(_angle),-sin(_angle),0.,
                sin(_angle),cos(_angle),0.,
                0.,0.,1.
               );
}

mat3 scale(vec2 _scale){
    return mat3(_scale.x,0.,0.,
                0.,_scale.y,0.,
                0.,0.,1.
               );
}

mat3 translate(vec2 t){
    return mat3(1.,0.,t.x,
                0.,1.,t.y,
                0.,0.,1.
               );
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Repeat around the origin by a fixed angle.
// For easier use, num of repetitions is use to specify the angle.
float pModPolar(inout vec2 p, float repetitions) {
    float angle = 2.*PI/repetitions;
    float a = atan(p.y, p.x) + angle/2.;
    float r = length(p);
    float c = floor(a/angle);
    a = mod(a,angle) - angle/2.;
    p = vec2(cos(a), sin(a))*r;
    // For an odd number of repetitions, fix cell index of the cell in -x direction
    // (cell index would be e.g. -5 and 5 in the two halves of the cell):
    if (abs(c) >= (repetitions/2.)) c = abs(c);
    return c;
}

// signed distance to an equilateral triangle
float sTri(vec2 p, float radius)
{
    const float k = sqrt(3.0);
    
    p.x = abs(p.x) - radius;
    p.y = p.y + radius / k;
    
    if( p.x + k*p.y > 0.0 ) p = vec2( p.x - k*p.y, -k*p.x - p.y )/2.0;
    
    p.x -= clamp( p.x, radius * -2., 0.0 );
    
    return -length(p)*sign(p.y);
}


float tex(vec2 p) {
    
    float r = .03;

    float d = sTri(p, r);

    pR(p, PI * 1./6.);
    pModPolar(p, 3.);
    float d2 = dot(p, vec2(1,0)) - r * .5;
    
    d = mix(d, d2, 1.);
    
    d = pow(d, 2.) * 10.;
    
    float w = 1. - smoothstep(0., .15, d);
    
    return clamp(w, .0, .9);
}

float sLine(vec2 p, vec2 a, vec2 b) {
  vec2 dir = b - a;
  return abs(dot(normalize(vec2(dir.y, -dir.x)), a - p));
}

vec3 drawShape(vec2 p, vec2 a, vec2 b, vec2 point, float weight) {
    float d = 1e12;
    float part;
    
    part = sLine(p, a, point);
    d = part;

    part = sLine(p, b, point);
    d = max(-d, -part);
    
    float minWeight = length(a - b) * .5;
    float maxWeight = 0.;
    d += mix(minWeight, maxWeight, weight);

    return vec3(smoothstep(.002, .0, d)); 
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    mat2 view = mat2(
        1. / iResolution.x, 0,
        0, 1. / iResolution.y / (iResolution.x / iResolution.y)
    );
    
    vec2 offset = iResolution.xy / 2.;
    
    vec2 uv = (fragCoord.xy - offset) * view;
    uv.y += .04;

    mat3 tr = scale(vec2(guiScale));
    tr *= rotate2d(guiRotation * PI);
    tr *= translate(vec2(guiOffsetX, guiOffsetY) * guiScale * .1);
    mat3 itr = inverse(tr);

    vec2 shapeUv = (vec3(uv, 1)* tr).xy;
    Vec23 result = shapePoints(shapeUv);

    result.a = (vec3(result.a, 1) * itr).xy;
    result.b = (vec3(result.b, 1) * itr).xy;
    result.c = (vec3(result.c, 1) * itr).xy;

    vec2 center = mix(result.a, result.b, .5);
    float weight = tex(center);
    vec3 color = drawShape(uv, result.a, result.b, result.c, weight);

    fragColor = vec4(color ,1.);
}
