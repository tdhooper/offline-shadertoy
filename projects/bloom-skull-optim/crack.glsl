uniform sampler2D noiseTexture; // images/noise.png filter: nearest wrap: repeat

#extension GL_EXT_shader_texture_lod : enable

vec2 hash2(vec2 p) {
   // return fract(sin(vec2(dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3))))*43758.5453);
    return texture2DLodEXT( noiseTexture, -(p+0.5)/256.0, 0.).xy;
    // return texture2D(noiseTexture, (p+0.5)/256.0).xy;
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
    
    if (bound > .01) {
    	return bound;
    }

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

    // use less samples, introduces errors
    //float d = max(fLine(p, p1, p2) * inv,fLine(p, p2, p3) * inv) * inv;

    float w = weight;
    
    float weightRamp = (1. - p.x / size.x / 2.);
    weight *= weightRamp;
    d = (abs(d) - weight);
    d = max(d, bound);
        
    return d;
}

#pragma glslify: export(fCrack)
