#define SHADER_NAME quad.frag

precision highp float;

uniform vec2 iResolution;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


/* SHADERTOY FROM HERE */

#pragma glslify: renderSuperstructure = require(./shaders/intergalactic.glsl, iChannel0=iChannel0)

// --------------------------------------------------------
// Rotation
// --------------------------------------------------------

#define PI 3.14159265359

mat3 sphericalMatrix(vec2 thetaphi) {
    float theta = -thetaphi.y;
    float phi = thetaphi.x;
    float cx = cos(theta);
    float cy = cos(phi);
    float sx = sin(theta);
    float sy = sin(phi);
    return mat3(
        cy, -sy * -sx, -sy * cx,
        0, cx, sx,
        sy, cy * -sx, cy * cx
    );
}



// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

struct Model {
    float dist;
};
    
// checks to see which intersection is closer
Model map(vec3 p){
    //p += vec3(1.5, 1.5, 0.);
    p = mod(p, 2.);
    p -= vec3(1.);
    float d = length(p) - .4;
    return Model(d);
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = 1.; // Default is 1, reduce to fix overshoots

struct CastRay {
    vec3 origin;
    vec3 direction;
};

struct Ray {
    vec3 origin;
    vec3 direction;
    float len;
};

struct Hit {
    Ray ray;
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 color;
    float steps;
};

vec3 calcNormal( in vec3 pos ){
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
    return normalize(nor);
}

Hit raymarch(CastRay castRay){
	float steps = 0.;
    float currentDist = INTERSECTION_PRECISION * 2.0;
    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        if (currentDist < INTERSECTION_PRECISION || ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        model = map(ray.origin + ray.direction * ray.len);
        currentDist = model.dist;
        ray.len += currentDist * FUDGE_FACTOR;
        steps += 1.;
    }

    bool isBackground = false;
    vec3 pos = vec3(0);
    vec3 normal = vec3(0);
    vec3 color = vec3(0);

    if (ray.len > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = ray.origin + ray.direction * ray.len;
        normal = calcNormal(pos);
    }

    return Hit(ray, model, pos, isBackground, normal, color, steps);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------


void shadeSurface(inout Hit hit) {
    vec3 background = vec3(1.);
    
    if (hit.isBackground) {
        hit.color = background;
        return;
    }

    hit.color = vec3(1);
    hit.color = hit.normal * .5 + .5;
}

vec4 render(Hit hit) {
	shadeSurface(hit);
    return vec4(hit.color, hit.ray.len);
}


// --------------------------------------------------------
// Camera
// https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

void doCamera(out vec3 camPos, out vec3 camTar, out float camRoll, in vec2 mouse) {
    float dist = 10.5;
    camRoll = 0.;
    camTar = vec3(0,0,0);
    camPos = vec3(0,0,dist);
    vec2 mousexy = (iMouse.xy / iResolution.xy - .5);
    if (mousexy.x < .3 || mousexy.y < .25) {
    	camPos *= sphericalMatrix(mousexy * 5.);
    }
    camPos += camTar;
}



void mainImage( out vec4 fragColor, in vec2 fragCoord )
{

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    #ifdef SHOW_ZOOM
    	p *= .7;
    #endif
    vec2 m = iMouse.xy / iResolution.xy;

    vec3 camPos = vec3( 0., 0., 2.);
    vec3 camTar = vec3( 0. , 0. , 0. );
    float camRoll = 0.;

    // camera movement
    doCamera(camPos, camTar, camRoll, m);

    // camera matrix
    mat3 camMat = calcLookAtMatrix( camPos, camTar, camRoll );  // 0.0 is the camera roll

    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length

    CastRay ray = CastRay(camPos, rd);
    Hit hit = raymarch(ray);

    vec4 model = render(hit);
    fragColor = model;
    
    vec4 sliderVal = vec4(0.5,0.4,0.16,0.7);
   	   
    // Super Structure
	vec4 col = renderSuperstructure(ray.origin, ray.direction, sliderVal, model);

    fragColor = col;
    
}

