vec3 GIZMO_LOCAL_P;

void GIZMO(vec3 p) {
    GIZMO_LOCAL_P = p;
}

float map(vec3 p) {

    float d = 1e12;

    d = min(d, length(p) - .5);

    p -= vec3(0, 1, -2);

    GIZMO(p);

    d = min(d, length(p) - .25);

    return d;
}

#pragma glslify: export(map)
