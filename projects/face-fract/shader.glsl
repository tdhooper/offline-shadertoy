uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform vec2 iChannel0Size;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

uniform float guiTransformX;
uniform float guiTransformY;
uniform float guiTransformZ;
uniform float guiRotateX;
uniform float guiRotateY;
uniform float guiRotateZ;
uniform float guiScale;
uniform int guiIterations;
uniform float guiTime;

#pragma glslify: import('./quat.glsl')


float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}


float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

bool FLESH = false;

// #define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

bool isMapPass = false;
bool isBound = false;

// Read head sdf from '3D' texture
float mHead(vec3 p) {
    p.x = -abs(p.x);
    p += OFFSET / SCALE;
    float bound = fBox(p, 1./SCALE);
    if (bound > .4) {
        return bound;
    }
    p *= SCALE;
    float d = mapTex(iChannel0, p, iChannel0Size.xy);
    d += max(0., bound);
    return d;
}

float g_disp;

vec3 projectOnPlane(vec3 v, vec3 n) {
    float scalar = dot(n, v) / length(n);
    vec3 v1 = n * scalar;
    return v - v1;
}


struct Transform {
    vec3 translate;
    vec4 rotate;
    float scale;
};
    
Transform mix(Transform t1, Transform t2, float t) {
    return Transform(
        mix(t1.translate, t2.translate, t),
        q_slerp(t1.rotate, t2.rotate, t),
        mix(t1.scale, t2.scale, t)
    );
}

Transform reverseTransform(Transform t) {
    t.translate *= -1.;
    t.rotate = q_conj(t.rotate);
    t.scale = 1. / t.scale;
    return t;
}

void applyTransform(inout vec3 p, Transform t) {
    p += t.translate;
    p = rotate_vector(p, t.rotate);
    p /= t.scale;
}

void applyTransformR(inout vec3 p, Transform t) {
    p /= t.scale;
    p = rotate_vector(p, t.rotate);
    p += t.translate;
}

struct Scene {
    int iterations;
    float blend;
    Transform origin;
    Transform fractal;
};

Scene scenes[5];
Scene scenesRev[5];

Scene reverseScene(Scene scene) {
    Transform origin = scene.origin;
    vec3 translate = scene.fractal.translate;
    for (int i = 0; i < 20; i++) {
        if (i > scene.iterations - 2) {
            break;
        }
        origin.translate += translate * origin.scale;
        origin.scale *= scene.fractal.scale;
        origin.rotate = qmul(origin.rotate, scene.fractal.rotate);
        translate = rotate_vector(translate, q_conj(scene.fractal.rotate));
    }
    Transform fractal = reverseTransform(scene.fractal);
    return Scene(
        scene.iterations,
        scene.blend,
        origin,
        fractal
    );
}

Scene mix(Scene s1, Scene s2, float t) {
    return Scene(
        int(round(mix(float(s1.iterations), float(s2.iterations), t))),
        mix(s1.blend, s2.blend, t),
        mix(s1.origin, s2.origin, t),
        mix(s1.fractal, s2.fractal, t)
    );
}

void calcScenes() {

    Transform origin = Transform(
        vec3(0,0,0),
        QUATERNION_IDENTITY,
        1.
    );

    scenes[0] = Scene(
        2, 0.,
        origin,
        origin
    );
    scenesRev[0] = reverseScene(scenes[0]);
    
    scenes[1] = Scene(
        3, .1,
        origin,
        Transform(
            vec3(-.4,0,0),
            q_euler(-.3, -.3, .3),
            .6
        )
    );
    scenesRev[1] = reverseScene(scenes[1]);

    scenes[2] = Scene(
        17, .02,
        origin,
        Transform(
            vec3(-.55,-.3,-.1)*.6,
            q_euler(-.3, .45, .15),
            .8
        )
    );
    scenesRev[2] = reverseScene(scenes[2]);

    scenes[3] = Scene(
        17, .02,
        origin,
        Transform(
            vec3(-.3,-.15,-.05),
            q_euler(-.3, .4, .3),
            .8
        )
    );
    scenesRev[3] = reverseScene(scenes[3]);

    scenes[4] = Scene(
        guiIterations, .02,
        origin,
        Transform(
            vec3(guiTransformX, guiTransformY, guiTransformZ),
            q_euler(guiRotateX * PI, guiRotateY * PI, guiRotateZ * PI),
            guiScale
        )
    );
    scenesRev[4] = reverseScene(scenes[4]);

}

float ss(float t) { return smoothstep(0., 1., t); }

float map(vec3 p) {
    
    p.y -= .22;
    p.z += .4;

    p.x = -p.x;
    vec3 pp = p;
    
    float d = 1e12;
    float scale = 1.;
    bool flip;
        
    float tt = mod(iTime / 3., 1.);
    // tt = guiTime;
    float t = sin(tt * PI);

    Scene start = scenes[0];
    Scene end = scenes[4];
    Scene scene = mix(start, end, t);

    float zt = sin(min(tt * PI * 2., PI / 2.));
    zt = .5 + min(.5, smoothstep(.5, 1.5, tt)) - smoothstep(.5, -.5, tt);
    zt = range(
        start.origin.scale,
        scenesRev[4].origin.scale,
        pow(scenesRev[4].origin.scale, zt)
    );

    // zt = smoothstep(.0, 1., zt);
    // zt = ss(zt);
    // zt = ss(zt);
    // zt = ss(zt);
    Transform origin = mix(start.origin, scenesRev[4].origin, zt);
    Transform origin2 = mix(start.origin, scenesRev[4].origin, tt);
    origin = reverseTransform(origin);
    origin2 = reverseTransform(origin2);
    // applyTransformR(p, origin);

    p /= origin.scale;
    p = rotate_vector(p, origin.rotate);
    p += origin.translate;

    bool reverse = tt > .5;

    if (reverse) {
        start = scenesRev[4];
        end = scenesRev[0];
        scene = mix(start, end, 1. - t);
    }

    applyTransform(p, start.origin);
    scale = start.origin.scale;
    for (int i = 0; i < 20; i++) {
        d = smin(d, mHead(p) * scale, scale * scene.blend);
        if (i > scene.iterations - 2) {
            break;
        }
        if (reverse) {
            applyTransformR(p, scene.fractal);
        } else {
            applyTransform(p, scene.fractal);
        }
        scale *= scene.fractal.scale;
    }
    
    return d * origin.scale;
}


const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    vec3 npos;
    for (int i = 0; i < NORMAL_STEPS; i++){
        npos = pos + eps * invert;
        nor += map(npos) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

// https://www.shadertoy.com/view/lsKcDD
float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    return 1.;
    float res = 1.0;
    float t = mint * 0.;
    
    for( int i=0; i<128; i++ )
    {
        float h = map( ro + rd*t );
        res = min( res, 10.0*h/t );
        t += h;
        if( res<0.0001 || t>tmax ) break;
        
    }
    return clamp( res, 0.0, 1.0 );
}

// https://www.shadertoy.com/view/Xds3zN
float calcAO( in vec3 pos, in vec3 nor )
{
    return 1.;
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos );
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    calcScenes();

    vec3 camPos = eye;
    vec3 rayDirection = dir;

    vec3 rayPosition = camPos;
    float rayLength = 0.;
    float dist = 0.;
    bool bg = false;

    for (int i = 0; i < 300; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        dist = map(rayPosition);

        if (abs(dist) < .001) {
            break;
        }
        
        if (rayLength > 10.) {
            bg = true;
            break;
        }
    }

    vec3 col = vec3(.19,.19,.22).zxy * .0;
    
    if ( ! bg) {
        vec3 pos = rayPosition;
        vec3 rd = rayDirection;
        vec3 nor = calcNormal(rayPosition);
        vec3 ref = reflect(rd, nor);
        vec3 up = normalize(vec3(1));

        // lighitng
        // IQ - Raymarching - Primitives 
        // https://www.shadertoy.com/view/Xds3zN
        float hole = range(4., 1., g_disp);
        float occ = calcAO( pos, nor ) * mix(.5, 1., hole);
        vec3  lig = normalize( vec3(-2., 2., .5) );
        vec3  lba = normalize( vec3(.5, 1., -.5) );
        vec3  hal = normalize( lig-rd );
        float amb = sqrt(clamp( 0.5+0.5*nor.y, 0.0, 1.0 ));
        float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
        float bac = clamp( dot( nor, lba ), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
        float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );
        
        dif *= softshadow( pos, lig, 0.01, .5 ) * hole;

        float spe = pow( clamp( dot( nor, hal ), 0.0, 1.0 ),16.0)*
                    dif *
                    (0.04 + 0.96*pow( clamp(1.0+dot(hal,rd),0.0,1.0), 5.0 ));

        vec3 lin = vec3(0.0);
        lin += 2.80*dif*vec3(1.30,1.00,0.70);
        lin += 0.55*amb*vec3(0.40,0.60,1.15)*occ;
        lin += 1.55*bac*vec3(0.25,0.25,0.25)*occ;
        lin += 0.25*fre*vec3(1.00,1.00,1.00)*occ;

        col = vec3(1, 0.8, 0.8) * .3;
        col = mix(col, vec3(.4,.05,.03) * .5, range(.0, .15, g_disp));
        col = mix(col, vec3(1,.0,.05) * .2, range(.2, .5, g_disp));
        col = mix(col, vec3(.05,0,0), range(.4, .5, g_disp));

        col = col*lin;
        col += 5.00*spe*vec3(1.10,0.90,0.70);
    }

    col = pow( col, vec3(0.4545) );

    fragColor = vec4(col,1.0);
}
