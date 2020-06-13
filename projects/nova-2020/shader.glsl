precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#pragma glslify: inverse = require(glsl-inverse)

#define PI 3.1415926




vec2 round(vec2 a) {
    return floor(a + .5);
}


void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}




struct Model {
    float d;
};

Model newModel() {
    return Model(1e12);
}

Model opU(Model a, Model b) {
    Model m = a;
    if (b.d < a.d) {
        m = b;
    }
    return m;
}




float time;


Model leaf(vec3 p, vec3 cellData) {
    
    vec2 cell = cellData.xy;
    float t = cellData.z;
    
    float d = 1e12;

    vec3 pp = p;

    Model model = newModel();

    float bound = length(p) - 1.;

    d = length(p.xy) - mix(.01, .05, t);
    d = max(d, -p.z);

    d = max(d, bound);

    model.d = d;
    return model;
}

vec3 calcCellData(
    vec2 cell,
    vec2 offset,
    mat2 worldToGrid,
    mat2 gridToWorld,
    float stretch,
    vec2 minmax
) {
    // Snap to cell center and move to neighbour
    cell = gridToWorld * (round(worldToGrid * cell) + offset);

    // Clamp first and last cell
    float o = .5 / stretch;
    cell.y = clamp(cell.y, minmax.x + o, minmax.y - o);
    cell = gridToWorld * round(worldToGrid * cell);

    // Calc cell time
    float t = 1. - (cell.y - minmax.x) / (minmax.y - minmax.x);

    return vec3(cell, t);
}

mat2 phyllotaxis;
void calcPhyllotaxis() {
    vec2 cc = vec2(5., 8.);
    float aa = atan(cc.x / cc.y);
    float scale = (PI*2.) / sqrt(cc.x*cc.x + cc.y*cc.y);
    mat2 mRot = mat2(cos(aa), -sin(aa), sin(aa), cos(aa));
    mat2 mScale = mat2(1./scale,0,0,1./scale);
    phyllotaxis = mRot * mScale;
}

Model drawBloom(
    vec3 p,
    float density,
    vec2 minmax
) {
    Model model = newModel();

    float stretch = density;
    vec2 cell = vec2(
        atan(p.x, p.z),
        atan(p.y, length(p.xz))
    );
    
    minmax = (minmax - .5) * PI;

    mat2 mStretch = mat2(1,0,0,stretch);
    mat2 worldToGrid = phyllotaxis * mStretch;
    mat2 gridToWorld = inverse(worldToGrid);

    vec3 cellData;
    vec2 leafCell;
    vec3 pp = p;

    // compile speed optim from IQ
    for( int m=0; m<3; m++ )
    for( int n=0; n<3; n++ )
    {
        cellData = calcCellData(cell, vec2(m,n)-1., worldToGrid, gridToWorld, stretch, minmax);
        p = pp;
        pR(p.xz, -cellData.x);
        pR(p.zy, cellData.y);
        model = opU(model, leaf(p, cellData));
    }

    return model;
}



Model drawBloom2(
    vec3 p,
    float density,
    vec2 minmax
) {
    Model model = newModel();

    float stretch = density;
    vec2 cell = vec2(
        atan(p.x, p.z),
        p.y
    );
    
    //minmax = (minmax - .5) * PI;

    mat2 mStretch = mat2(1,0,0,stretch);
    mat2 worldToGrid = phyllotaxis * mStretch;
    mat2 gridToWorld = inverse(worldToGrid);

    vec3 cellData;
    vec2 leafCell;
    vec3 pp = p;

    // compile speed optim from IQ
    for( int m=0; m<3; m++ )
    for( int n=0; n<3; n++ )
    {
        cellData = calcCellData(cell, vec2(m,n)-1., worldToGrid, gridToWorld, stretch, minmax);
        p = pp;
        pR(p.xz, -cellData.x);
        p.y -= cellData.y;
        //pR(p.zy, cellData.y);
        model = opU(model, leaf(p, cellData));
    }

    return model;
}






float map(vec3 p) {
    //Model m = drawBloom(p, iTime * 5., vec2(.5, .7));
    Model m = drawBloom2(p, iTime * 5., vec2(.0, 2.));
    float d = length(p) - .01;
    d = min(d, m.d);
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
    vec3 col;
    calcPhyllotaxis();

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
    bool bg = true;

    for (int i = 0; i < 100; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        dist = map(rayPosition);

        if (abs(dist) < .001) {
            bg = false;
            break;
        }
        
        if (rayLength > 50.) {
            break;
        }
    }

    col =  vec3(.19,.19,.22);
    
    if ( ! bg) {
        vec3 nor = calcNormal(rayPosition);
        col = nor * .5 + .5;
    }

    col = pow( col, vec3(0.4545) );
    fragColor = vec4(col,1.0);
}
