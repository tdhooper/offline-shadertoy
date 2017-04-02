// Created by sebastien durand - 11/2016
//-------------------------------------------------------------------------------------
// Based on "Type 2 Supernova" by Duke (https://www.shadertoy.com/view/lsyXDK) 
// Sliders from IcePrimitives by Bers (https://www.shadertoy.com/view/MscXzn)
// License: Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License
//-------------------------------------------------------------------------------------

const float GAMMA = 2.2;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}

vec3 screenToLinear(vec3 screenRGB) {
    return gamma(screenRGB, GAMMA);
}

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 nebulaPal(float t) {
    return screenToLinear(pal(t, vec3(1.,.9,1.),vec3(0.4,0.3,0.),vec3(1.5,1.5,0.),vec3(.2,0.05,0.0)));    
}


void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}



#define SPIRAL_NOISE_ITER 5

float hash( const in vec3 p ) {
    return fract(sin(dot(p,vec3(127.1,311.7,758.5453123)))*43758.5453123);
}

float pn(in vec3 x) {
    vec3 p = floor(x), f = fract(x);
	f *= f*(3.-f-f);
	vec2 uv = (p.xy+vec2(37,17)*p.z) + f.xy,
	     rg = texture2D( iChannel0, (uv+.5)/256.).yx;
	return 2.4*mix(rg.x, rg.y, f.z)-1.;
}

//-------------------------------------------------------------------------------------
// otaviogood's noise from https://www.shadertoy.com/view/ld2SzK
//--------------------------------------------------------------
// This spiral noise works by successively adding and rotating sin waves while increasing frequency.
// It should work the same on all computers since it's not based on a hash function like some other noises.
// It can be much faster than other noise functions if you're ok with some repetition.
const float nudge = 20.;	// size of perpendicular vector
float normalizer = 1.0 / sqrt(1.0 + nudge*nudge);	// pythagorean theorem on that perpendicular to maintain scale
float SpiralNoiseC(vec3 p, vec4 id) {
    float iter = 2., n = 2.-id.x; // noise amount
    for (int i = 0; i < SPIRAL_NOISE_ITER; i++) {
        // add sin and cos scaled inverse with the frequency
        n += -abs(sin(p.y*iter) + cos(p.x*iter)) / iter;	// abs for a ridged look
        // rotate by adding perpendicular and scaling down
        p.xy += vec2(p.y, -p.x) * nudge;
        p.xy *= normalizer;
        // rotate on other axis
        p.xz += vec2(p.z, -p.x) * nudge;
        p.xz *= normalizer;  
        // increase the frequency
        iter *= id.y + .733733;
    }
    return n;
}

float map(vec3 p, vec4 id) {
    //p += iGlobalTime;
    p *= .5;
   // p *= mix(.5, 1., sin(iGlobalTime) * .5 + .5);
    //p += vec3(6., 7., 6.) * iGlobalTime * .2;
    p += 1000.;
    //p.xy -= mousee * 20.;

    p.xy -= (vec2(0.4446096695045556, 0.4677165305520606) - .5) * 20.;
    p.y -= .1;
    //p.xy -= (vec2(0.6328301654671723, 0.17047622090294248) - .5) * 20.;
    //p *= 2.;
    //float limit = dot(normalize(p), vec3(0,0,1)) - 2.;
    float k = 2.*id.w +.1; //  p/=k;
    float d = k*(.5 + SpiralNoiseC(p.zxy*.4132+333., id)*3. + pn(p*8.5)*.12);
    return d;
    //return max(d, -limit);
}

//-------------------------------------------------------------------------------------
// Based on [iq: https://www.shadertoy.com/view/MsS3Wc]
//-------------------------------------------------------------------------------------
vec3 hsv2rgb(float x, float y, float z) {	
	return z+z*y*(clamp(abs(mod(x*6.+vec3(0,4,2),6.)-3.)-1.,0.,1.)-1.);
}




float cubicPulse( float c, float w, float x )
{
    x = abs(x - c);
    if( x>w ) return 0.;
    x /= w;
    return 1. - x*x*(3.-2.*x);
}


//-------------------------------------------------------------------------------------
// Based on "Type 2 Supernova" by Duke (https://www.shadertoy.com/view/lsyXDK) 
//-------------------------------------------------------------------------------------
vec4 renderSuperstructure(vec3 ro, vec3 rd, const vec4 id, vec4 model) {
    const float max_dist=50.;
	float ld, td=0., w, d, t, noi, lDist, a,         
    	  rRef = 2.*id.x,
          h = .05+.25*id.z;
   
    vec3 pos, lightColor;   
    vec4 sum = vec4(0);
    
    lDist = 0.;

    float alphaMultiplier = .0;
   	
    float clipNear = 8. * (sin(iGlobalTime * 3.) * .5 + .5);
    clipNear = 7.5;
    float clipFar = mix(8., 15., sin(iGlobalTime/2.) *.5 + .5);
    clipFar = 20.9;
    float clipBlend = (clipFar - clipNear) / 2.;

    //t = .3*hash(vec3(hash(rd))); 
    t = clipNear;

    for (int i=0; i<200; i++)  {
		// Loop break conditions.
	    if(/*td>.9 ||  */sum.a > .99 || t>max_dist) break;
        
        if (t > clipFar) {
            break;
        } 

        if (t > model.w) {
          break;
        }
        
        // Color attenuation according to distance
        a = smoothstep(max_dist,0.,t);

        vec3 pos = ro + t*rd;
        vec3 ppos = pos;

        // Evaluate distance function
        d = abs(map(pos, id))+.07;
        
        // Light calculations
        float lightRepeat = 5.;
        float lightOffset = lightRepeat / 2.;
        //pR(pos.xz, iGlobalTime);
        //pos += iGlobalTime;
        pos = mod(pos + lightOffset, lightRepeat);
        lDist = length(pos - lightOffset);
        //lDist = max(lDist, .001); // TODO add random offset
        //lDist = max(length(pos) - lightOffset, .001); // TODO add random offset
        
        pos = ppos;

        noi = pn(0.03*pos);
        lightColor = mix(hsv2rgb(noi,.5,.6), 
                         hsv2rgb(noi+.3,.5,.6), 
                         smoothstep(rRef*.5,rRef*2.,lDist));
       
        float lightBlend = sin(lDist * lightRepeat * .08 + .8) * .5 + .5; 
//        lightColor = mix(vec3(.3,.1,1), vec3(1.,.2,.7), lightBlend);
        lightColor = nebulaPal(lightBlend);


        alphaMultiplier = clamp((t - clipNear) * .5, 0., 1.);
        
        alphaMultiplier = smoothstep((t - clipNear) * .5, clipNear, clipFar);

        alphaMultiplier = cubicPulse(clipNear + clipBlend, clipBlend, t);

        alphaMultiplier *= 1.;

        //if (t > 10.) {
            sum.rgb += (a * lightColor * .025) * alphaMultiplier;
            //float contrib = .002 * lDist;
            //sum += vec4(vec3(contrib), .02 * alphaMultiplier);
            sum.a += .02 * alphaMultiplier;
        //}

        if (d<h) {
			td += (1.-td)*(h-d)+.005;  // accumulate density
            sum.rgb += sum.a * sum.rgb * .15 / lDist;  // emission	
			sum += (1.-sum.a)*.05*td*a;  // uniform scale density + alpha blend in contribution 
        } 
		
        td += .015;
        t += max(d * .08 * max(min(lDist,d),2.), .01);  // trying to optimize step size
    }
    //return model;
    //return vec4(sum.rgb, 1.);

    float bl = 1. - smoothstep(dot(rd, vec3(0,0,-1)), 1., .98);
    bl = max(bl, .5);
    //sum.a *= bl;
    sum = mix(model, sum, sum.a);
    sum.a = 1.;

    //return vec4(vec3(bl), 1.);
 
    // simple scattering
    //sum *= 1. / exp(lDist*.2)*.9;
   	//sum = clamp(sum, 0., 1.);   
    //sum.xyz *= sum.xyz*(3.-sum.xyz-sum.xyz);
	return sum;
}

#pragma glslify: export(renderSuperstructure)
