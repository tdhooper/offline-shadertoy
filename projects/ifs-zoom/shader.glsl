precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;
uniform vec4 iMouse;

uniform float guiScale;
uniform float guiRotX;
uniform float guiRotY;
uniform float guiRotZ;
uniform float guiOffsetX;
uniform float guiOffsetY;
uniform float guiOffsetZ;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;

#pragma glslify: inverse = require(glsl-inverse)

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


/*

	Logarithmic Conical Spiral Center and Axis
	------------------------------------------

	When iteratively applying a transformtion matrix, the
	positions visited form a logarithmic spiral on a cone.
	
	This calculates the center (tip) and axis of that cone,
	as well as the angle between each iteration.

	The general approach is to ignore the scaling component
	of the matrix, such that multiple iterations form a
	cylinder instead of a cone. Once we have the axis and
	position of the cylinder, we can triangulate the tip of
	the cone.

	With these parameters, we can create a method that
	rotates and scales space over time to smoothly transform
	the second instance into the first, forming a
	seamless loop.

	See also the 2d version:
	https://www.shadertoy.com/view/tscBDH

*/

// Matrix functions
// --------------------------------------------------------

mat4 mTranslate(vec3 t) {
	return mat4(
        1, 0, 0, t.x,
        0, 1, 0, t.y,
        0, 0, 1, t.z,
        0, 0, 0, 1
   	);
}

mat4 mRotate(vec3 forward, vec3 up) {
    vec3 ww = normalize(forward * vec3(1,1,-1));
    vec3 uu = normalize(cross(up * vec3(-1,1,1),ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat4(
        uu, 0,
        vv, 0,
        ww, 0,
        0, 0, 0, 1
    );
}

mat4 rotX(float a) {
    return mat4(1,0,0,0,  0,cos(a),-sin(a),0,  0,sin(a),cos(a),0,  0,0,0,1);
}

mat4 rotY(float a) {
    return mat4(cos(a),0,sin(a),0,  0,1,0,0,  -sin(a),0,cos(a),0,  0,0,0,1);
}

mat4 rotZ(float a) {
    return mat4(cos(a),-sin(a),0,0,  sin(a),cos(a),0,0,  0,0,1,0,  0,0,0,1);
}

mat4 mScale(float s) {
	return mat4(
    	s, 0, 0, 0,
        0, s, 0, 0,
        0, 0, s, 0,
        0, 0, 0, 1
    );
}

float getScale(mat4 m) {
	return length(m[0].xyz);
}

vec3 mul(vec3 p, mat4 m) {
	return (vec4(p, 1) * m).xyz;
}


// Logarithmic conical spiral center, axis, and angle
// --------------------------------------------------------

#define PI 3.1415926

mat3 basisMatrix(vec3 axis, vec3 up) {
    vec3 ww = normalize(axis);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(uu, vv, ww);
}

// Logarithmic spiral center
// https://www.shadertoy.com/view/tscBDH
vec2 spiralCenter(vec2 translation, float rotation, float scale) { 
    float l = sqrt(scale * scale - 2. * scale * cos(rotation) + 1.);
    float S = sin(rotation) * scale / l;
    float C = sqrt(1. - S * S);
    return mat2(-S, C, C, S) * translation.yx / l;
}

void conicalSpiral(mat4 txm, float txmScale, out vec3 axis, out float angle, out vec3 center) {

    // Remove scaling from transformation matrix
    mat4 txmns = mScale(1. / txmScale) * txm;
    
    // Get first four positions, these form a cylinder
    vec3 v0 = vec3(0);
    vec3 v1 = mul(v0, txmns);
    vec3 v2 = mul(v1, txmns);
    vec3 v3 = mul(v2, txmns);
    

	// Cylinder axis
    // -------------
    
    // Calculate normals for the two middle points
    vec3 n0 = v1 - mix(v0, v2, .5);
    vec3 n1 = v2 - mix(v1, v3, .5);

    // Cross for cylinder axis
    axis = normalize(cross(n0, n1));

    // Rotation matrix for cylinder direction
    mat3 mAxis = basisMatrix(axis, vec3(0,1,0));


  	// Angle between iterations
    // ------------------------

    // Project points onto axis plane
    vec2 p1 = (v1 * mAxis).xy;
    vec2 p2 = (v2 * mAxis).xy;

    // Angle between the points
    angle = PI - acos(dot(normalize(-p1), normalize(p2 - p1)));    
    

	// Cone center (tip)
	// -----------------

    // Center of the 2d logarithmic spiral
    vec2 center2d = spiralCenter(p1, angle, txmScale);

    // Transform back into 3d
    center = vec3(center2d, 0) * inverse(mAxis);
    
    // Extrapolate the line between v0 and v1 to find the tip
    float v1Height = dot(v1, axis);
    float v1Radius = distance(center2d, p1);
    center += axis * v1Height * (length(center2d) / (length(center2d) - v1Radius));
}


// Camera movement
// --------------------------------------------------------

mat4 txm;
mat4 txmi;
float txmScale;

vec3 cameraAxis;
float cameraAngle;
vec3 cameraApex;

// Rotate around axis
// blackle https://suricrasia.online/demoscene/functions/
vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(ax, p)*ax, p, cos(ro)) + cross(ax,p)*sin(ro);
}

// With the calculated spiral properties, rotate and scale space
// (aka move the camera), by a given amount until we have transformed
// the second iteration into the first iteration
float tweenCamera(inout vec3 p, float t) {
    float scale = pow(txmScale, t);
    p -= cameraApex;
    p = erot(p, cameraAxis, cameraAngle * t);
    p *= scale;    
    p += cameraApex;
    return scale;
}


// Modeling
// --------------------------------------------------------

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fLine(vec3 p, vec3 n) {
    float t = dot(p, n) / dot(n, n);
    return length((n * t) - p) ;
}

struct Model {
	float d;
    vec3 p;
    int id;
};

vec2 smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return vec2((1. - f) * a + f  * b - f * (1. - f) * k, f);
}

Model opU(Model a, Model b, float scale) {
    //vec2 dc = smin(a.d, b.d, 2.4 * scale);    
    //return Model(dc.x, mix(a.p, b.p, dc.y), a.id);
    if (a.d < b.d) return a;
    return b;
}

float time;

Model mGizmo(vec3 p) {
 // p = mul(p, rotY(time * PI * 2.));
  //p.z -= .2;
  float s = .5;

    vec3 sz = vec3(3., 1., 1.) * s;

	float d = fBox(p, sz);

    //d = max(d, dot(p, normalize(vec3(0,1,1))) - .6);

  //d = max(d, length(p) - 1. * s);
    //d = length(p);
   	return Model(d, (p / sz) * vec3(-1,-1,1), 1);
}

Model mAxisAndCenter(vec3 p) {
    float axis = fLine(p - cameraApex, cameraAxis) - .08;
    float center = length(p - cameraApex) - .3;
    float d = min(axis, center);
   	return Model(d, p, 2);
}


Model map(vec3 p) {


    float ball = length(p) - .001;

  p -= cameraApex;


    float t = time;

  	Model model = Model(1e12, p, 0);
    
    float scale = 1.;
    vec3 pp = p;

  p = abs(p);
  
  
    for (int i = 0; i < 0; i++) {
    	p = mul(p, txm);
    	scale /= txmScale;
      //p = abs(p);
    }
    
  

    
    float camScale = tweenCamera(p, t);


    float scl;
    const int n = 24;
    
        float orbitTrap = 1e20;

    bool isMirror = false;

   // p = abs(p);

    for (int i = 0; i < n; i++) {
        
        scl = 1.;
        
        
        if (i == n - 1) {
        	scl = t;
        }

        if (i == 0 || isMirror) {
        //	scl *= 1. - t;
        }
        

        
        
        if (scl <= 0.) break;
        
        // Draw gizmo
	    Model gizmo = mGizmo(p / scl);
        gizmo.p.x = float(i) - t;
        gizmo.d *= scale * scl; // Fix distance for scale factor
       // gizmo.d += (1. - scl) * 10.1 * scale;
        model = opU(model, gizmo, scale * scl);
        
        // Apply matrix and scale
        p = mul(p, txmi);
        scale *= txmScale;
        //p.xy = abs(p.xy);


        //if (p.x < 0.) {
        //  p.x = -p.x;
        //  isMirror = isMirror || i == 0;
        //}

        p = abs(p);
       
        orbitTrap = min(orbitTrap, length(p)-scale);

    }


    model.d /= camScale;

   // model.d = min(model.d, ball);
    return model;
}



// Rendering
// --------------------------------------------------------

vec3 calcNormal( in vec3 p ) // for function f(p)
{
    const float eps = 0.0001; // or some other value
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy).d - map(p-h.xyy).d,
                           map(p+h.yxy).d - map(p-h.yxy).d,
                           map(p+h.yyx).d - map(p-h.yyx).d ) );
}


// Spectrum palette, iq https://www.shadertoy.com/view/ll2GD3
vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}
vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
 

    time = mod(iTime / 2., 1.);

    //t = 2.5;
    //t = 1. - t;

    //t = 0.;
    
    // Build the TRS transformation matrix for each iteration
    mat4 mT = mTranslate(vec3(guiOffsetX,guiOffsetY,guiOffsetZ));
    //mat4 mR = mRotate(vec3(.2,-.2,-1. + sin(t+.5) * .3), vec3(.2,.3,1));
    mat4 mR = rotX(guiRotX * PI * 2.) * rotY(guiRotY * PI * 2.) * rotZ(guiRotZ * PI * 2.);
    mat4 mS = mScale(guiScale);
	txm = mS * mR * mT;
    txmScale = getScale(txm);
	txmi = inverse(txm);    

    // Find conical spiral parameters for camera movement
    conicalSpiral(txm, txmScale, cameraAxis, cameraAngle, cameraApex);
    
    
    // Ray marching

    float focalLength = 2.4;
    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;

    vec3 rayDirection = normalize(vec3(p, -focalLength));
    vec3 rayOrigin = vec3(0,0,10);

    rayOrigin = eye;
    rayDirection = normalize(vec3(p.x * fov, p.y * fov, -1.) * mat3(vView));

    vec3 camUp = vec3(0,1,0) * mat3(vView);

    vec3 rayPosition = rayOrigin;
    float rayLength = 0.;

    Model model;
    float dist = 0.;
    vec3 bgcol = vec3(0.);
    vec3 color = bgcol;
    bool isBackground = false;
    float glow = 0.;

    vec2 hash = hash22(p);

    for (float i = 0.; i < 100.; i++) {
        rayLength += dist;
        rayPosition = rayOrigin + rayDirection * rayLength;
        model = map(rayPosition);
		dist = model.d;
        dist += model.d * (hash.x * 2. - 1.) * .01;

        glow += 1.;

        hash = hash22(hash);
        
        if (dist < .0001) {
            break;
        }

        if (rayLength > 50.) {
            isBackground = true;
            break;
        }
    }



    if ( ! isBackground) {
        if (model.id == 2) {
        	color = vec3(1);
        } else {
    		  vec3 face = step(vec3(vmax(abs(model.p))), abs(model.p)) * sign(model.p);
          float faceIndex = max(vmax(face * vec3(0,1,2)), vmax(face * -vec3(3,4,5)));
    		  color = spectrum(faceIndex / 6.);
              //color = vec3(.5);
          vec3 nor = calcNormal(rayPosition);
          color = (nor.yxz * .5 + .5);
          color = mix(color, color.yxz, smoothstep(14., 15., model.p.x));
          //color = spectrum(dot(rayDirection, nor) * .75 - .1);
          color *= clamp(dot(nor, camUp) * .75 + .25, 0., 1.);
    	}
    }
    
    float fog = 1. - exp(rayLength * -16. + 2.);
    color += clamp(pow(glow, 2.) * .00025, 0., 1.);
    //color = mix(color, bgcol, clamp(fog, 0., 1.));
    color = mix(color, vec3(1) * length(color * .01), clamp(fog, 0., 1.));


    color = pow(color, vec3(1. / 2.2)); // Gamma

    color += (hash.y * 2. - 1.) * .005;

    fragColor = vec4(color, 1);
}
