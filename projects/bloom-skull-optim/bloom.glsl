
Model leaf(vec3 p, vec3 cellData, float thickness, float pointy, float width, bool shrinkOuter) {
    
    vec2 cell = cellData.xy;
    float cellTime = cellData.z;
    
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

    if (shrinkOuter) {
        len *= mix(.2, 1., rangec(-.5, .0, cell.y));
    }

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
        wedge2 = smax(wedge2, dot(p, n * vec3(1,1,-1)), .1);
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
        
        float wedgeT = smax(d2, wedge, thickness);
        float wedgeT2 = smax(d2, wedge2, thickness);
        d = mix(wedgeT2, smin(wedgeT, wedgeT2, .01), pointy);
        wedges = mix(wedge2, wedge2, pointy);
        
        p = pp;
        len = llen;
        vec2 uv = p.xz / len;

        model.p = p;
        model.d = d;
        model.isBloom = true;
        model.uv = uv;
        model.cell = cell;
        model.wedges = mix(wedge2, smin(wedge, wedge2, .01), pointy);
        model.slice = slice;
        model.len = len;
        model.neg = smax(max(slice, top), wedges, .01);
        return model;
    }

    model.d = d;
    model.p = p;
    return model;
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

    // Hide leaves outside the growth area
    cell = transformI * cell;
    cell.y *= stretch / sz / stretchStart;
    cell.y = max(cell.y, 4.4/stretchEnd); // clamp, not sure why this magic number
    if (hideInside) {
        cell.y = min(cell.y, 1.21/stretchStart); // clamp, not sure why this magic number
    }
    cell.y /= stretch / sz / stretchStart;
    cell = transform * cell;

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
    vec2 density,
    float thickness,
    float pointy,
    float width,
    bool hideInside
) {
    pR(p.xz, .7);

    Model model = newModel();

    t = rangec(-.1, 1., t);

    p.y -= mix(0., .25, t);

    vec2 move = vec2(0, t);
    float stretchStart = density.x;
    float stretchEnd = density.y;
    float stretch = mix(stretchStart, stretchEnd, t);
    float maxBloomOffset = PI / 5.;

    vec2 cell = vec2(
        atan(p.x, p.z),
        atan(p.y, length(p.xz)) + maxBloomOffset
    );

    mat2 mStretch = mat2(1,0,0,stretch);
    mat2 transform = phyllotaxis * mStretch;
    mat2 transformI = inverse(transform);
    bool shrinkOuter = hideInside;

    // compile speed optim from IQ
    for( int m=ZERO; m<3; m++ )
    for( int n=ZERO; n<3; n++ )
    {
        model = opU(model, leaf(
            p,
            calcCellData(cell, vec2(m,n)-1., maxBloomOffset, transform, transformI, stretch, stretchStart, stretchEnd, t, hideInside),
            thickness,
            pointy,
            width,
            shrinkOuter
        ));
    }

    return model;
}
