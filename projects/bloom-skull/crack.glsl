uniform sampler2D noiseTexture; // images/noise.png filter: nearest wrap: repeat

#extension GL_EXT_shader_texture_lod : enable

vec2 hash2(vec2 p) {
   // return fract(sin(vec2(dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3))))*43758.5453);
    return texture2DLodEXT( noiseTexture, -(p+0.5)/256.0, 0.).xy;
    // return texture2D(noiseTexture, (p+0.5)/256.0).xy;
}

float hash(vec2 p) {
    return hash2(p).x;
}


float noise( in vec2 p )
{
    vec2 i = floor( p );
    vec2 f = fract( p );
	
	vec2 u = f*f*(3.0-2.0*f);

    return mix( mix( hash( i + vec2(0.0,0.0) ), 
                     hash( i + vec2(1.0,0.0) ), u.x),
                mix( hash( i + vec2(0.0,1.0) ), 
                     hash( i + vec2(1.0,1.0) ), u.x), u.y);
}

float fbm(vec2 uv) {
    uv *= 8.0;
    mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
    float f = 0.;
    f  = 0.5000*noise( uv ); uv = m*uv;
    f += 0.2500*noise( uv ); uv = m*uv;
    f += 0.1250*noise( uv ); uv = m*uv;
    f += 0.0625*noise( uv ); uv = m*uv;
    return f;
}

float fLine(vec2 p, vec2 a, vec2 b) {
	return dot(p - a, normalize(b - a) * mat2(0,-1,1,0));
}

vec2 getPoint(float x, int offset, float seed, vec2 size, vec2 scale) {
   	float cell = (floor(x / scale.x) + float(offset));
    float inv = mod(cell, 2.) * 2. - 1.;
   	vec2 pos = hash2(vec2(cell, seed));
    pos.y *= inv;
    pos.y *= scale.y;
    float ramp = cell / size.x * scale.x / 2.;
    pos.y *= 1. - ramp;
   	pos += vec2(cell, 0.);
    pos.x *= scale.x;
    return pos;
}

float fCrack(vec2 p, vec2 size, float lines, float seed, float weight) {
    
    float bound = max(-p.x - weight/2., p.x - size.x * 2.);
    vec2 scale = vec2(size.x / lines * 4., size.y);
    
//    cc = floor(x);

    vec2 p0 = getPoint(p.x, -2, seed, size, scale);
    vec2 p1 = getPoint(p.x, -1, seed, size, scale);
    vec2 p2 = getPoint(p.x, 0, seed, size, scale);
    vec2 p3 = getPoint(p.x, 1, seed, size, scale);
    vec2 p4 = getPoint(p.x, 2, seed, size, scale);
    
    float inv = mod(floor(p.x / scale.x), 2.) * 2. - 1.;
    
    float d = max(
        min(
        	fLine(p, p0, p1) * inv,
        	fLine(p, p1, p2) * inv
        ),
        min(
        	fLine(p, p2, p3) * inv,
        	fLine(p, p3, p4) * inv
        )
    ) * inv;
    
    float w = weight;
    
    float weightRamp = (1. - p.x / size.x / 2.);
    weight *= weightRamp;
    d = (abs(d) - weight);
    //weightRamp = max(weightRamp, .01);
    //d = min(d, d / weightRamp);
    d = max(d, bound);
    
    float dbg = smoothstep(.3, .0, abs(d));
    // d = mix(d, d + (fbm(p.xx*5.) * 2. - 1.) * .25 * w, 1.);
        
    return d;
    
}

#pragma glslify: export(fCrack)
