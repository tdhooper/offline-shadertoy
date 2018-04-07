
// https://www.shadertoy.com/view/ldK3zD

//
// interesting part starts here
//
// the meter uses the "fusion" gradient, which goes from dark magenta (0) to white (1)
// (often seen in heatmaps in papers etc)
//

#define PI 3.14159265359

vec3 fusion(float x) {
    float t = clamp(x,0.0,1.0);
    return clamp(vec3(sqrt(t), t*t*t, max(sin(PI*1.75*t), pow(t, 12.0))), 0.0, 1.0);
}

// HDR version
vec3 fusionHDR(float x) {
    float t = clamp(x,0.0,1.0);
    return fusion(sqrt(t))*(0.5+2.*t);
}


//
// distance meter function. needs a bit more than just the distance
// to estimate the zoom level that it paints at.
//
// if you have real opengl, you can additionally use derivatives (dFdx, dFdy)
// to detect discontinuities, i had to strip that for webgl
//
// visualizing the magnitude of the gradient is also useful
//

vec3 distanceMeter(float dist, float rayLength, vec3 rayDir, float camHeight) {
    float idealGridDistance = 20.0/rayLength*pow(abs(rayDir.y),0.8);
    float nearestBase = floor(log(idealGridDistance)/log(10.));
    float relativeDist = abs(dist/camHeight);
    
    float largerDistance = pow(10.0,nearestBase+1.);
    float smallerDistance = pow(10.0,nearestBase);

   
    vec3 col = fusionHDR(log(1.+relativeDist));
    col = max(vec3(0.),col);
    if (sign(dist) < 0.) {
        col = col.grb*3.;
    }

    float l0 = (pow(0.5+0.5*cos(dist*PI*2.*smallerDistance),10.0));
    float l1 = (pow(0.5+0.5*cos(dist*PI*2.*largerDistance),10.0));
    
    float x = fract(log(idealGridDistance)/log(10.));
    l0 = mix(l0,0.,smoothstep(0.5,1.0,x));
    l1 = mix(0.,l1,smoothstep(0.0,0.5,x));

    col.rgb *= 0.1+0.9*(1.-l0)*(1.-l1);
    return col;
}


//
// interesting part ends here
//

#pragma glslify: export(distanceMeter)
