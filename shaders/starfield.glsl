
const int STARFIELD_VOL_STEPS = 20;
const int STARFIELD_ITERATIONS = 20;

// https://www.shadertoy.com/view/XlfGRj
vec3 starField(vec2 uv, vec3 offset) {

    float zoom, speed, formuparam, stepsize,
        tile, brightness, darkmatter,
        distfading, saturation;
    
	zoom = .500;
    formuparam = 0.53;
	stepsize = 0.050;
	tile = 1.950;
	brightness = 0.0020;
	darkmatter = 0.000;
	distfading = 0.790;
	saturation = 0.950;
    
	vec3 dir=vec3(uv*zoom,1.);
	vec3 from=vec3(1.,.5,0.5);
	from+=offset;

	float anim = (tile * 2.) * time;
	from.xy += vec2(1,-1) * anim;

    float sampleShift = mod( from.z, stepsize );
	float zoffset = -sampleShift;
	sampleShift /= stepsize; // make from 0 to 1
	
	



	//volumetric rendering
	float s=0.1;
	vec3 v=vec3(0.);
	for (int r=0; r<STARFIELD_VOL_STEPS; r++) {
		vec3 p=from+(s+zoffset)*dir;// + vec3(0.,0.,zoffset);
		
	//pR(p.xz, time * 3.142 * 2.);

		p = abs(vec3(tile)-mod(p,vec3(tile*2.))); // tiling fold
		float pa,a=pa=0.;
		for (int i=0; i<STARFIELD_ITERATIONS; i++) { 
			p=abs(p)/dot(p,p)-formuparam; // the magic formula
			//p=abs(p)/max(dot(p,p),0.005)-formuparam; // another interesting way to reduce noise
            float D = abs(length(p)-pa); // absolute sum of average change
            D = i > 10 ? min( 5., D) : D;
            a += D;
			pa=length(p);
		}
		//float dm=max(0.,darkmatter-a*a*.001); //dark matter
		a*=a*a; // add contrast
		//if (r>3) fade*=1.-dm; // dark matter, don't render near
		// brightens stuff up a bit
		float s1 = s+zoffset;
		// need closed form expression for this, now that we shift samples
		float fade = pow(distfading,max(0.,float(r)-sampleShift));
		v+=fade;
        
		// fade out samples as they approach the camera
		if( r == 0 )
			fade *= 1. - sampleShift;
		// fade in samples as they approach from the distance
		if( r == STARFIELD_VOL_STEPS-1 )
			fade *= sampleShift;
		v+=vec3(2.*s1,4.*s1*s1,16.*s1*s1*s1*s1)*a*brightness*fade; // coloring based on distance
		s+=stepsize;
	}

	v=mix(vec3(length(v)),v,saturation); //color adjust
    return v;
}

#pragma glslify: export(starField)
