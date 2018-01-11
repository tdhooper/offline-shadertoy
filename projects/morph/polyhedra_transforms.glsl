

// --------------------------------------------------------
// POLYHEDRA TRANSFORMS
// knighty https://www.shadertoy.com/view/MsKGzw
// --------------------------------------------------------

struct Tri {
    vec3 a;
    vec3 b;
    vec3 c;
};
    
struct TriPlanes {
    vec3 ab;
    vec3 bc;
    vec3 ca;
};

struct TriMetrics {
    vec3 a;
    vec3 b;
    vec3 c;
    vec3 ao; // a opposite (bc midpoint)
    vec3 bo; // b opposite (ca midpoint)
    vec3 co; // c opposite (ab midpoint)
    vec3 ad; // a direction
    vec3 bd; // b direction
    vec3 cd; // c direction
};

vec3 nc,pab,pbc,pca;
Tri tri;
TriPlanes triP;

float cospin;
float scospin;

void initPolyhedron(int Type) {//setup folding planes and vertex
    cospin=cos(PI/float(Type));
    scospin=sqrt(0.75-cospin*cospin);
    nc=vec3(-0.5,-cospin,scospin);//3rd folding plane. The two others are xz and yz planes
    pab=vec3(0.,0.,1.);
    pbc=vec3(scospin,0.,0.5);//No normalization in order to have 'barycentric' coordinates work evenly
    pca=vec3(0.,scospin,cospin);
    pbc=normalize(pbc); pca=normalize(pca);//for slightly better DE. In reality it's not necesary to apply normalization :) 
    tri = Tri(pbc, pab, pca);    
    triP = TriPlanes( 
        normalize(cross(tri.a, tri.b)),
        normalize(cross(tri.b, tri.c)),
        normalize(cross(tri.c, tri.a))
    );
}

// Repeat space to form subdivisions of an icosahedron
void pTetrahedron(inout vec3 p) {
    initPolyhedron(3);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
}

// Repeat space to form subdivisions of an icosahedron
void pHexahedron(inout vec3 p) {
    initPolyhedron(4);
    p = abs(p);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
}

// Repeat space to form subdivisions of an icosahedron
void pIcosahedron(inout vec3 p) {
    initPolyhedron(5);
    p = abs(p);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
}

vec3 slerp(vec3 start, vec3 end, float percent) {
    //end = normalize(end);
    //start = normalize(start);
     float dot = dot(start, end);     
     dot = clamp(dot, -1.0, 1.0);
     float theta = acos(dot)*percent;
     vec3 RelativeVec = normalize(end - start*dot);
     return ((start*cos(theta)) + (RelativeVec*sin(theta)));
}

// Barycentric to Cartesian
vec3 bToC(float a, float b, float c) {
    return a * tri.a + b * tri.b + c * tri.c;
}
vec3 bToC(int a, int b, int c) {
    return bToC(float(a), float(b), float(c));
}

// Barycentric to Cartesian normalized
vec3 bToCn(float a, float b, float c) {
    return normalize(bToC(a, b, c));
}
vec3 bToCn(int a, int b, int c) {
    return bToCn(float(a), float(b), float(c));
}
