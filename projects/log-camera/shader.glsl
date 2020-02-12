

#pragma glslify: inverse = require(glsl-inverse)
#pragma glslify: import('./quat.glsl')
#pragma glslify: import('./camera.glsl')


float time;

float fPart(vec3 p) {
    float a = fBox(p, vec3(.05, .4, .1));
    float b = fBox(p - vec3(-.1, .3, .1), vec3(.1,.02,.02));
    return min(a, b);
    return length(p) - .2;
}


float map(vec3 p) {
    float camScale = tweenCamera(p, time);
    float w = mapWaypoints(p);

    float d = 1e12;
    float part;
    float scale = 1.;
    float flip = 1.;

    mat3 rot = -basisMatrix(-stepForward, stepUp);

    for (float i = 0.; i < 4.; i++) {
        part = fPart(p) / scale;
        d = min(d, part);

        p -= stepPosition;
        p /= stepScale;
        p *= rot;

        flip *= -1.;

        scale /= stepScale;
    }

    d = min(d, w);

    d /= camScale;

    return d;
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

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    stepRotate = basisMatrix(stepForward, stepUp);
    stepRotate2 = -basisMatrix(-stepForward, stepUp);

    calcWaypoints();

    vec3 col;

    float mTime = mod(iTime / 1., 1.);
    time = mTime;

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;

    vec3 camPos = eye;
    vec3 rayDirection = normalize(dir);

    // mat3 camMat = calcLookAtMatrix( camPos, vec3(0,.23,-.35), -1.68);
    // rayDirection = normalize( camMat * vec3(p.xy,2.8) );

    vec3 rayPosition = camPos;
    float rayLength = 0.;
    float dist = 0.;
    bool bg = false;

    for (int i = 0; i < 300; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        dist = map(rayPosition);

        if (abs(dist) < .00001) {
            break;
        }
        
        if (rayLength > 60.) {
            bg = true;
            break;
        }
    }

    col =  vec3(.19,.19,.22) * .5;
    
    if ( ! bg) {
        vec3 nor = calcNormal(rayPosition);
        col = nor * .5 + .5;
    }

    col = pow( col, vec3(0.4545) );
    fragColor = vec4(col,1.0);
}
