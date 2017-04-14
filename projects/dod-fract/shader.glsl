precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */



#define MODEL_ROTATION vec2(.5, .5)
#define CAMERA_ROTATION vec2(.5, .5)

// 0: Defaults
// 1: Model
// 2: Camera
#define MOUSE_CONTROL 1

//#define DEBUG

float time;

#define PI 3.14159265359
#define TAU 6.28318530718


// --------------------------------------------------------
// Rotation controls
// --------------------------------------------------------

mat3 sphericalMatrix(float theta, float phi) {
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

mat3 mouseRotation(bool enable, vec2 xy) {
    if (enable) {
        vec2 mouse = iMouse.xy / iResolution.xy;

        if (mouse.x != 0. && mouse.y != 0.) {
            xy.x = mouse.x;
            xy.y = mouse.y;
        }
    }
    float rx, ry;

    rx = (xy.y + .5) * PI;
    ry = (-xy.x) * 2. * PI;

    return sphericalMatrix(rx, ry);
}

mat3 modelRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==1, MODEL_ROTATION);
    return m;
}

mat3 cameraRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==2, CAMERA_ROTATION);
    return m;
}


// --------------------------------------------------------
// HG_SDF
// --------------------------------------------------------

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smax(float a, float b, float r) {
    float m = max(a, b);
    if ((-a < r) && (-b < r)) {
        return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
    } else {
        return m;
    }
}

float smin(float a, float b, float r) {
	float m = min(a, b);
	if ((a < r) && (b < r) ) {
		return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
	} else {
	 return m;
	}
}

// Cone with correct distances to tip and base circle. Y is up, 0 is in the middle of the base.
float fCone(vec3 p, float radius, float height) {
	vec2 q = vec2(length(p.xz), p.y);
	vec2 tip = q - vec2(0, height);
	vec2 mantleDir = normalize(vec2(height, radius));
	float mantle = dot(tip, mantleDir);
	float d = max(mantle, -q.y);
	float projected = dot(tip, vec2(mantleDir.y, -mantleDir.x));
	
	// distance to tip
	if ((q.y > height) && (projected < 0.)) {
		d = max(d, length(tip));
	}
	
	// distance to base ring
	if ((q.x > radius) && (projected > length(vec2(height, radius)))) {
		d = max(d, length(q - vec2(radius, 0)));
	}
	return d;
}

float fCone(vec3 p, float radius, float height, vec3 direction, float offset) {
    p -= direction * offset;
    p = reflect(p, normalize(mix(vec3(0,1,0), -direction, .5)));
    //p -= vec3(0,height,0);
    return fCone(p, radius, height);
}

// --------------------------------------------------------
// Icosahedral domain mirroring
// knighty https://www.shadertoy.com/view/MsKGzw
// 
// Also get the face normal, and tangent planes used to
// calculate the uv coordinates later.
// --------------------------------------------------------

#define PI 3.14159265359

vec3 facePlane;
vec3 uPlane;
vec3 vPlane;

int Type=5;
vec3 nc;
vec3 pab;
vec3 pbc;
vec3 pca;

void init() {
    float cospin=cos(PI/float(Type)), scospin=sqrt(0.75-cospin*cospin);
    nc=vec3(-0.5,-cospin,scospin);
    pbc=vec3(scospin,0.,0.5);
    pca=vec3(0.,scospin,cospin);
    pbc=normalize(pbc); pca=normalize(pca);
	pab=vec3(0,0,1);
    
    facePlane = pca;
    uPlane = cross(vec3(1,0,0), facePlane);
    vPlane = vec3(1,0,0);
}

void fold(inout vec3 p) {
	for(int i=0;i<5 /*Type*/;i++){
		p.xy = abs(p.xy);
		p -= 2. * min(0., dot(p,nc)) * nc;
	}
}



// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

struct Model {
    float dist;
    float id;
};
    
// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}


float sineInOut(float t) {
  return -0.5 * (cos(PI * t) - 1.0);
}

float sineOutIn(float t) {
  return asin(t * 2. - 1.) / PI + .5;
}

float easeWithRest(float t) {
	float blend = cos(t * PI * 2.) * -.5 + .5;
    float slope = .2;
    float lin = (t - .5) * slope + .5;
    return mix(sineInOut(t), lin, blend);
}

float bDelay(float delay, float duration, float loop) {
    float t = mod(time, loop);
    if (t < delay) {
    	return 0.;
    }
    if (t > duration + delay) {
    	return 1.;
    }
    t -= delay;
    t /= duration;
    return t;
    //return sineOutIn(t);
    //return easeWithRest(t);
    //return sineInOut(t);
}

float LOOP = 3.5;

float makeModel(vec3 p, float scale) {
    float subScale = mix(1., .5, time / 6.);
    //subScale = 1.;
    p /= subScale;
    float d = length(p) - .1;
    return d * subScale * scale;
}

void makeSpace(inout vec3 p, float scale, float startTime) {
    //p /= scale;
    fold(p);
    float duration = 1.;
    float blend = max(0., time - startTime) / duration;
    float moveMax = .5 * scale;
    p -= facePlane * mix(.0, moveMax, blend);
}

float subDModel3(vec3 p, float scale, float startTime) {
    makeSpace(p, scale, startTime);
    return makeModel(p, scale);    
}

float subDModel2(vec3 p, float scale, float startTime) {
    makeSpace(p, scale, startTime);
    return subDModel3(p, scale * 1., startTime + 2.);    
}

float subDModel(vec3 p, float scale, float startTime) {
    makeSpace(p, scale, startTime);
    return subDModel2(p, scale * 1., startTime + 2.);    
}


float modelC(vec3 p) {
	float d;
    fold(p);
    p -= facePlane * mix(.0, .2, bDelay(0., LOOP, LOOP));
 	return length(p) - .04;
}

float modelB(vec3 p) {
    fold(p);
 	//p -= facePlane * 1.23;
    p -= facePlane * mix(.3, .9, bDelay(0., LOOP, LOOP));
	float a = length(p) - .15;
    float b = modelC(p);
    //return a;
    return mix(a, b, easeWithRest(bDelay(2.2, 1., LOOP)));
}

float modelA(vec3 p) {
    fold(p);
 	p -= facePlane * mix(1.5, 3., bDelay(0., LOOP, LOOP));
    float a = length(p) - .55;
    float b = modelB(p);
    return mix(a, b, easeWithRest(bDelay(1.1, 1., LOOP)));
}

float modelX(vec3 p) {
    float a = length(p) - 2.3;
    float b = modelA(p);
    return mix(a, b, easeWithRest(bDelay(0., 1., LOOP)));
}


Model map( vec3 p ){
    mat3 m = modelRotation();
    p *= m;
    time = mod(time, 6.);
    Model model = Model(subDModel(p, 1., 0.), 1.);
    return model;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = .8; // Default is 1, reduce to fix overshoots

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

    return Hit(ray, model, pos, isBackground, normal, color);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1);

    if (hit.isBackground) {
        hit.color = background;
        return;
    }
    

    vec3 light = normalize(vec3(.5,1,0));
    vec3 diffuse = vec3(dot(hit.normal, light) * .5 + .5);
    
    vec3 colA = vec3(.1,.75,.75);
    vec3 colB = vec3(.75,.1,.75);
    
    float blend = sin(hit.model.id * TAU * 3.) * .5 + .5;
    
    diffuse *= mix(colA, colB, blend);
    diffuse = sin(diffuse);
    diffuse *= 1.3;
    
    float fog = clamp((hit.ray.len - 5.) * .5, 0., 1.);
    
    diffuse = mix(diffuse, background, fog);
    
    
    hit.color = diffuse;
}


vec3 render(Hit hit){

#ifdef DEBUG
    return hit.normal * .5 + .5;
#endif

    shadeSurface(hit);

    return hit.color;
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
    float dist = 2.;
    camRoll = 0.;
    camTar = vec3(0,0,0);
    camPos = vec3(0,0,-dist);
    camPos *= cameraRotation();
    camPos += camTar;
}


// --------------------------------------------------------
// Gamma
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

const float GAMMA = 1.;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    init();
    
    time = iGlobalTime;
    //time /= 2.;
    //time = mod(time, 1.);

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
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

    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);

    #ifndef DEBUG
       color = linearToScreen(color);
    #endif

    fragColor = vec4(color,1.0);
}
