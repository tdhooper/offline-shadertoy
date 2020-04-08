

Model leaf(vec3 p, vec3 cellData, float thickness, float pointy, float width) {
    //cellData = vec3(0,0,.1);
    
    vec2 cell = cellData.xy;
    float cellTime = cellData.z;
    
    //cell.x = 0.;
    //cell.y = .1;
    //cellTime = .2;

    float d = 1e12;
    float d2 = 1e12;
    float slice = 1e12;
    float wedge, wedges;

    // orient
    pR(p.xz, -cell.x);
    pR(p.zy, cell.y);

    vec3 pp = p;

    cellTime = max(cellTime, 0.);

    float core = length(p) - .1;

    float len = max(cellTime*3. - .2, 0.);
    len = pow(len, .33);
    float llen = len;

    Model model = newModel();


    if (cellTime > 0.) {


        // wedge
        float ins = .25;
        p.z += ins;
        vec3 n = normalize(vec3(1,0,.35));
        wedge = -dot(p, n);
        wedge = max(wedge, dot(p, n * vec3(1,1,-1)));
        wedge = smax(wedge, p.z - len*1.12 - ins, len);
        p.z -= ins;


        // wedge2
        ins = .2;
        p.z += ins;
        n = normalize(vec3(1,0,width));
        float wedge2 = -dot(p, n);
        wedge2 = max(wedge2, dot(p, n * vec3(1,1,-1)));
        wedge2 = smax(wedge2, p.z - len*.95 - ins, len*.6);
        p.z -= ins;

        float r = len / 8.;

        float top = p.y - len * .5;
        float curve = smoothstep(0., .2, cellTime);

        len *= mix(1.5, .65, curve);
        pR(p.zy, -mix(.2, .7, curve));
        slice = length(p - vec3(0,len,0)) - len;
        d2 = abs(slice) - thickness;
        d2 = max(d2, top);
        
        float d3 = smax(d2, wedge, thickness);
        float d4 = smax(d2, wedge2, thickness);
        wedges = smin(wedge, wedge2, .01);
        d3 = smin(d3, d4, .01);
        d = mix(d4, d3, pointy);
        
        p = pp;
        len = llen;
        vec2 uv = p.xz / len;

        model.p = p;
        model.d = d;
        model.isBloom = true;
        model.uv = uv;
        model.cell = cell;
        model.wedges = wedges;
        model.slice = slice;
        model.len = len;
        return model;
    }

    model.d = d;
    model.p = p;
    return model;
}

vec3 calcBloomAlbedo(Model model) {    
    vec3 col = vec3(.15,.15,.4);

    vec3 p = model.p;
    float len = model.len;
    vec2 cell = model.cell;
    float wedges = model.wedges;
    float slice = model.slice;
    vec2 uv = model.uv;
    
    float tip = length(p - vec3(0,.2,len*.9));

    tip = smoothstep(.5, .0, tip);
    tip *= smoothstep(.07, .0, abs(slice+.01));
    tip *= smoothstep(-.2, .0, wedges);
    tip = pow(tip, 1.5);
    col = mix(col, vec3(1,.2,.5), tip);

    float vs = 1.-uv.y*1.;
    vs *= smoothstep(.0, -.1, wedges);
    vs *= smoothstep(.0, .05, abs(slice));

    col *= mix(vec3(1), vec3(.5,5.,1.8), smoothstep(.2, 1.8, cell.y) * .75);
  
    return col;
}

vec3 calcCellData(
    vec2 cell,
    vec2 offset,
    float maxBloomOffset,
    mat2 transform,
    mat2 transformI,
    float stretch,
    float stretchStart,
    float stretchEnd,
    float t,
    bool hideInside
) {

    float sz = maxBloomOffset + PI / 2.;

    cell = transform * cell;

    // Snap to cell center
    cell = round(cell);
    cell += offset;

    if (hideInside) {
        // Hide leaves outside the growth area
        cell = transformI * cell;
        cell.y *= stretch / sz / stretchStart;
        cell.y = max(cell.y, .55/stretchStart); // clamp, not sure why this magic number
        cell.y = min(cell.y, 1.21/stretchStart); // clamp, not sure why this magic number
        cell.y /= stretch / sz / stretchStart;
        cell = transform * cell;
    }

    // Snap after clamp
    cell = round(cell);

    cell = transformI * cell;

    // calculate cell time
    float y = cell.y * (stretch / sz);
    float cellAppearTime = (stretchStart - y) / (stretchStart - stretchEnd);
    float cellTime = t - cellAppearTime;

    cell.y -= maxBloomOffset;

    return vec3(cell, cellTime);
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
    float t,
    float density,
    float thickness,
    float pointy,
    float width,
    bool hideInside
) {
    // t = mod(iTime, 1.);
    pR(p.xz, .7);

    Model model = newModel();

    float bound = length(p) - mix(.7, 1.5, t);
    if (bound > .01) {
        model.d = bound;
        return model;
    }

    t = rangec(-.1, 1., t);


    p.y -= mix(0., .25, t);

    vec2 move = vec2(0, t);
    float stretchStart = .25;
    float stretchEnd = density;
    float stretch = mix(stretchStart, stretchEnd, t);
    float maxBloomOffset = PI / 5.;

    vec2 cell = vec2(
        atan(p.x, p.z),
        atan(p.y, length(p.xz)) + maxBloomOffset
    );

    mat2 mStretch = mat2(1,0,0,stretch);
    mat2 transform = phyllotaxis * mStretch;
    mat2 transformI = inverse(transform);

    // compile speed optim from IQ
    for( int m=0; m<3; m++ )
    for( int n=0; n<3; n++ )
    {
        model = opU(model, leaf(
            p,
            calcCellData(cell, vec2(m, n) - 1., maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t, hideInside),
            thickness,
            pointy,
            width
        ));
    }

    return model;
}
