
float gTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
    p -= t;
    p = mix(dot(r.xyz,p)*r.xyz, p, cos(-r.w))+sin(-r.w)*cross(r.xyz,p);
    p /= s;
    return min(s.x, min(s.y, s.z));
}
