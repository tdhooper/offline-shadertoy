vec3 GIZMO_LOCAL_P;

void GIZMO(vec3 p) {
    GIZMO_LOCAL_P = p;
}

float map(vec3 p) {

    p -= 1.;

    GIZMO(p);

    return length(p) - .5;
}

#pragma glslify: export(map)
