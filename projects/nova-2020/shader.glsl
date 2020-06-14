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


vec2 hash2( vec2 p ){
    vec2 q = vec2( dot(p,vec2(127.1,311.7)), 
				   dot(p,vec2(269.5,183.3)));
	return fract(sin(q)*43758.5453);
}

float hash(vec2 p) { return fract(1e4 * sin(17.0 * p.x + p.y * 0.1) * (0.1 + abs(sin(p.y * 13.0 + p.x)))); }




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

struct BloomSpec {
    bool straight;
    float stretch;
    vec2 minmax;
    float size;
};


Model leaf(vec3 p, vec3 cellData, BloomSpec spec) {
    
    vec2 cell = cellData.xy;
    float t = cellData.z;
    
    float d = 1e12;

    vec3 pp = p;

    Model model = newModel();

    float bs = spec.straight ? t : mix(.5, 1., smoothstep(.0, .666, t));
    float bound = length(p) - spec.size * bs;

    d = length(p.xy) - mix(.01, .05, t);
    d = max(d, -p.z);

    d = max(d, bound);

    model.d = d;
    return model;
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

struct GridTransforms {
    mat2 worldToGrid;
    mat2 gridToWorld;
};

GridTransforms calcGridTransforms(float stretch) {
    mat2 worldToGrid = phyllotaxis * mat2(1,0,0,stretch);
    mat2 gridToWorld = inverse(worldToGrid);
    return GridTransforms(worldToGrid, gridToWorld);
}

vec3 calcCellData(
    vec2 cell,
    vec2 offset,
    GridTransforms m,
    BloomSpec spec
) {
    // Snap to cell center and move to neighbour
    cell = m.gridToWorld * (round(m.worldToGrid * cell) + offset);

    // Clamp first and last cell
    float o = .5 / spec.stretch;
    cell.y = clamp(cell.y, spec.minmax.x + o, spec.minmax.y - o);
    cell = m.gridToWorld * round(m.worldToGrid * cell);

    // Calc cell time
    float t = 1. - (cell.y - spec.minmax.x) / (spec.minmax.y - spec.minmax.x);

    return vec3(cell, t);
}

Model mBloom(
    vec3 p,
    BloomSpec spec
) {
    vec3 pp = p;
    vec2 cell = vec2(
        atan(p.x, p.z),
        spec.straight ? p.y : atan(p.y, length(p.xz))
    );
    spec.minmax = spec.straight ? spec.minmax : spec.minmax * PI / 2.;
    GridTransforms gridTransforms = calcGridTransforms(spec.stretch);
    Model model = newModel();
    //model.d = length(p) - .2; return model;
    for( int m=0; m<3; m++ )
    for( int n=0; n<3; n++ )
    {
        vec3 cellData = calcCellData(cell, vec2(m,n)-1., gridTransforms, spec);
        p = pp;
        pR(p.xz, -cellData.x);
        if (spec.straight) {
            p.y -= cellData.y;
        } else {
            pR(p.zy, cellData.y);
        }
        model = opU(model, leaf(p, cellData, spec));
    }
    return model;
}



const float sqrt3 = 1.7320508075688772;
const float i3 = 0.5773502691896258;

const mat2 cart2hex = mat2(1, 0, i3, 2. * i3);
const mat2 hex2cart = mat2(1, 0, -.5, .5 * sqrt3);


float map(vec3 p) {

    Model model = newModel();
    Model bloom;

    model.d = p.y;
    
    vec2 pFloor = floor(p.xz);
    vec2 pFract = fract(p.xz);

    mat2 sc = mat2(1,0,0,.785);
    mat2 sci = inverse(sc);

    vec2 hexP = p.xz * sc * cart2hex;
    vec2 hexPf = floor(hexP);
    vec3 pp = p;

    for (int j = -1; j <= 1; j++)
    for (int i = -1; i <= 1; i++) {
        vec2 cellId = hexPf + vec2(i, j);
        vec2 center = cellId * hex2cart * sci + (hash2(cellId) * 2. - 1.) * .2;
        p = pp - vec3(center.x,0,center.y);
        float r1 = hash(cellId + 5.5);
        float r2 = hash(cellId + 8.8);
        float sz = mix(.4, .7, r1);
        float n = mix(2., 8., r2);
        // if (r2 < .5) {
        //     sz *= .5;
        //     n *= .5;
        //     model = opU(model, mBloom(p + vec3(.2,0,0), BloomSpec(false, n, vec2(.0, 1.), sz)));
        //     model = opU(model, mBloom(p + vec3(-.2,0,-.23), BloomSpec(false, n, vec2(.0, 1.), sz)));
        //     model = opU(model, mBloom(p + vec3(-.2,0,.23), BloomSpec(false, n, vec2(.0, 1.), sz)));
        //     //model = opU(model, mBloom(p, BloomSpec(false, n, vec2(.0, 1.), sz)));
        //     //model = opU(model, mBloom(p, BloomSpec(false, n, vec2(.0, 1.), sz)));
        // } else {
            model = opU(model, mBloom(p, BloomSpec(false, n, vec2(.0, 1.), sz)));
        // }
    }

    return model.d;
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

    for (int i = 0; i < 300; i++) {
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
