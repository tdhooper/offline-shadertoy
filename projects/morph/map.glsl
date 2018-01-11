#pragma glslify: import('./hg_sdf.glsl')
#pragma glslify: import('./modelling_tools.glsl')
#pragma glslify: import('./polyhedra_transforms.glsl')



// Chunky dodecahedron
Model model1(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, o;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bump;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .9);
    
    n = bToC(0,0,1);
    spike = fCone(p, .4, n * 0., n * 2.5);
    n2 = reflect(n, triP.ab);
    spike2 = fCone(p, .4, n2 * 0., n2 * 2.5);
    outer = smin(outer, spike, .15);
    outer = smin(outer, spike2, .15);
    
    hole = fCone(p, 1.5, n * 2.6, n * .8);
    outer = smax(outer, -hole, .07);
    spike = fCone(p, 1.1, n * 0., n * 1.2) - .02;
    outer = smin(outer, spike, .01);
    
    n = bToC(0,1,0);
    spike = fCone(p, .9, n * 0., n * 1.1) - 0.02;
    outer = smin(outer, spike, .05);
    
    n = bToC(1,0,0);
    hole = fCone(p, .5, n * 1.5, n * -1.);
    outer = smax(outer, -hole, .05);
    
    n = bToC(0,0,1);
    bump = length(p - n * 1.1) - .3;
    inner = smin(inner, bump, .1);
    
    d = smax(outer, inner, .05);
    
    return Model(d, colour, 1.);
}

// Holey geodesic
Model model2(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, n4, n5, n6, o;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bump;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .95);
    
    n = bToC(1,0,0);
    spike = fCone(p, 1., n * 0., n * 1.05) - 0.04;
    outer = smin(outer, spike, .1);
    
    n = bToC(.15,.85,.0);
    spike = fCone(p, .6, n * 0., n * 1.1) - 0.02;
    outer = smin(outer, spike, .1);
    n = reflect(n, triP.bc);
    spike = fCone(p, .6, n * 0., n * 1.1) - 0.02;
    outer = smin(outer, spike, .1);
    
    float thickness = .025;
    float radius = .05;
    float bias = .64;
    n1 = bToC(bias, 1. - bias, 0.);
    n2 = bToC(1. - bias, bias, 0.);
    n5 = reflect(n2, triP.ca);
    n3 = reflect(n1, normalize(cross(n2, n5)));
    n4 = reflect(n3, triP.ca);
    n5 = reflect(n2, triP.ca);
    n6 = reflect(n1, triP.ca);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5, n6);
    outer = smax(outer, -hole, .03);

    radius = .08;
    n1 = n3;
    n6 = n4;
    n2 = reflect(n1, triP.bc);
    n3 = reflect(n6, triP.bc);
    n4 = reflect(n3, triP.ca);
    n5 = reflect(n4, triP.bc);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5, n6);
    outer = smax(outer, -hole, .03);
    
    d = smax(outer, inner, .03);
    //d = outer;
    
    return Model(d, colour, 1.);
}

Model model3(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, n4, n5, n6, o;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bump;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .95);
    
    n = bToCn(.65,.0,.35);
    spike = fCone(p, .25, n * 0., n * 1.13) - 0.01;
    outer = smin(outer, spike, .05);
    
    n = bToCn(.335,.0,.665);
    spike = fCone(p, .25, n * 0., n * 1.2) - 0.01;
    outer = smin(outer, spike, .05);
    
    n = bToCn(.0,.6,.4);
    spike = fCone(p, .25, n * 0., n * 1.08) - 0.01;
    outer = smin(outer, spike, .05);

    
    float thickness, radius, bias, bias2, bias3;
    float cutRadius = .04;
    
    thickness = .01;
    radius = .08;
    bias = .68;
    n2 = bToC(bias, 1. - bias, 0.);
    n3 = reflect(n2, triP.ca);
    n1 = reflect(n3, triP.ab);
    n4 = reflect(n1, triP.ca);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4);
    outer = smax(outer, -hole, cutRadius);

    thickness = .015;
    radius = .07;
    n4 = bToC(.5, .0, .5);
    n5 = n2;
    n2 = bToC(.0, 1., .0);
    n3 = bToC((1. - bias) / 2., bias / 2., .5);
    n1 = reflect(n3, triP.ab);
    n6 = reflect(n4, triP.ab);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5, n6);
    outer = smax(outer, -hole, cutRadius);
    
    thickness = .015;
    radius = .09;
    n3 = n3;
    n4 = reflect(n3, triP.bc);
    n2 = reflect(n3, triP.ca);
    n5 = reflect(n2, triP.bc);
    n1 = reflect(n4, triP.ca);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5);
    outer = smax(outer, -hole, cutRadius);
    
    d = smax(outer, inner, cutRadius);
    //d = outer;
    
    return Model(d, colour, 1.);
}


Model model4(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, n4, n5, n6, o;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, spikes2, bias, crop;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .95);
    
    n1 = bToCn(1,0,0);
    n2 = bToCn(0,0,1);
    n3 = reflect(n2, triP.ab);
    
    spike = fCone(p, .6, n1 * 0., n1 * 2.);
    outer = smin(outer, spike, .15);
    spike = fCone(p, .6, n2 * 0., n2 * 2.);
    outer = smin(outer, spike, .15);
    spike = fCone(p, .6, n3 * 0., n3 * 2.);
    outer = smin(outer, spike, .15);
    
    float shell = outer + .05;
    outer = max(outer, -shell);
    
    hole = fCone(p, .3, n1 * 2., n1 * -1.);
    outer = smax(outer, -hole, .05);
    hole = fCone(p, .3, n2 * 2., n2 * -1.);
    outer = smax(outer, -hole, .05);
    hole = fCone(p, .3, n3 * 2., n3 * -1.);
    outer = smax(outer, -hole, .05);

    bias = .7;
    n = bToCn(1. - bias,bias,.0);
    hole = fCone(p, .2, n * 2., n * -1.);
    outer = smax(outer, -hole, .02);
  
    bias = .5;
    n1 = bToCn(bias, 0., 1. - bias);
    bias = .9;
    n2 = bToCn(bias, 0., 1. - bias);
    hole = fCone(p, .09, n1 * 1.2, n2 * .95);
    outer = smax(outer, -hole, .02);
    
    bias = .5;
    n1 = bToCn(bias, 0., 1. - bias);
    bias = .2;
    n2 = bToCn(bias, 0., 1. - bias);
    hole = fCone(p, .09, n1 * 1.2, n2 * .95);
    outer = smax(outer, -hole, .02);
    
    bias = .2;
    n1 = bToCn(0., 1. - bias, bias);
    bias = .5;
    n2 = bToCn(0., 1. - bias, bias);
    hole = fCone(p, .09, n1 * 1.2, n2 * .95);
    outer = smax(outer, -hole, .02);

    d = outer;
    
    return Model(d, colour, 1.);
}

Model model5(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, n4, n5, n6, o;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bump;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .95);
    
    n = bToCn(.76,.0,.24);
    spike = fCone(p, .25, n * 0., n * 1.09) - 0.01;
    outer = smin(outer, spike, .05);
    
    n = bToCn(.335,.0,.665);
    spike = fCone(p, .25, n * 0., n * 1.18) - 0.01;
    outer = smin(outer, spike, .05);
    
    n = bToCn(.0,.6,.4);
    spike = fCone(p, .25, n * 0., n * 1.14) - 0.01;
    outer = smin(outer, spike, .05);

    
    float thickness, radius, bias, bias2, bias3;
    float cutRadius = .04;
    
    bias = .8;
    thickness = .0;
    radius = .05;
    n2 = bToC(bias, 1. - bias, 0.);
    n3 = reflect(n2, triP.ca);
    n1 = reflect(n3, triP.ab);
    n4 = reflect(n1, triP.ca);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4);
    outer = smax(outer, -hole, cutRadius);

    thickness = .015;
    radius = .1;
    n4 = bToC(.5, .0, .5);
    n5 = n2;
    n2 = bToC(.0, 1., .0);
    n3 = bToC((1. - bias) / 2., bias / 2., .5);
    n1 = reflect(n3, triP.ab);
    n6 = reflect(n4, triP.ab);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5, n6);
    outer = smax(outer, -hole, cutRadius);
    
    thickness = .0;
    radius = .1;
    n3 = n3;
    n4 = reflect(n3, triP.bc);
    n2 = reflect(n3, triP.ca);
    n5 = reflect(n2, triP.bc);
    n1 = reflect(n4, triP.ca);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5);
    outer = smax(outer, -hole, cutRadius);
    
    d = smax(outer, inner, cutRadius);
    //d = outer;
    
    return Model(d, colour, 1.);
}


Model model6(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, n4, n5, n6, o;
    d = 1000.;
        
    pTetrahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bump;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .9);
    
    float thickness, radius, bias1, bias2;
    
    bias1 = .6;
    bias2 = .48;
    
    n = bToCn(1,0,0);
    spike = fCone(p, .4, n * 0., n * 1.2) - 0.03;
    outer = smin(outer, spike, .15);
    
    n = bToCn(bias2,.0,1.-bias2);
    spike = fCone(p, .4, n * 0., n * 1.15) - 0.03;
    outer = smin(outer, spike, .15);
    
    n = bToCn(.0,bias1,1.-bias1);
    spike = fCone(p, .4, n * 0., n * 1.2) - 0.03;
    outer = smin(outer, spike, .15);

    thickness = .05;
    radius = .15;
    
    n1 = bToCn(0.,bias1,1.-bias1);
    n2 = bToCn(bias2,0.,1.-bias2);
    n3 = bToCn(1,0,0);
    n4 = reflect(n2, triP.ab);
    n5 = reflect(n1, triP.ab);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5);
    outer = smax(outer, -hole, .05);
    
    n2 = n2;
    n3 = n1;
    n4 = reflect(n2, triP.bc);
    n1 = reflect(n3, triP.ca);
    n5 = reflect(n1, triP.bc);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4, n5);
    outer = smax(outer, -hole, .05);
    
    d = smax(outer, inner, .05);
    
    return Model(d, colour, 1.);
}

Model model7(vec3 p) {

    float d, r;
    vec3 colour, c1, c2, n, n1, n2, n3, n4, n5, n6, o;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bias1, bias2;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .9);
    
    float thickness, radius;
    
    thickness = .005;
    radius = .2;

    n1 = bToCn(0,0,1);
    n2 = bToCn(1,0,0);

    n = n1;
    spike = fCone(p, .6, n * 0., n * 1.1) - 0.02;
    outer = smin(outer, spike, .05);
    
    n = n2;
    spike = fCone(p, .3, n * 0., n * 1.3) - 0.02;
    outer = smin(outer, spike, .15);
    
    n2 = bToCn(.65,.35,.0);
    n3 = reflect(n1, triP.ab);
    n4 = reflect(n2, triP.bc);
    hole = fPoly(p, thickness, radius, n1, n2, n3, n4);
    outer = smax(outer, -hole, .05);

    bias1 = .9;
    bias2 = .4;
    n1 = bToCn(bias1,.0,1.-bias1);
    n2 = bToCn(bias2,.0,1.-bias2);
    hole = fCone(p, .14, n2 * 1.3, n1 * .9) - 0.03;
    outer = smax(outer, -hole, .05);
    
    d = smax(outer, inner, .05);
    //d = outer;
    
    float bump;
    bump = length(p - n * 0.75) - .25;
    bump = smax(bump, (length(p) - 1.), .05);
    d = smax(d, -bump, .03);

    
    //d = 1000.;
    //float cap;
    //n1 = bToCn(1,0,0);
    //n2 = bToCn(.3,.0,.7);
    //cap = fCapsule(p, n1, n2, .02);
    //d = min(d, cap);
    //n3 = bToCn(.0,.5,.5);
    //cap = fCapsule(p, n2, n3, .02);
    //d = min(d, cap);
    
    
    return Model(d, colour, 1.);
}



Model model8(vec3 p) {

    float d;
    vec3 n, n1;
    d = 1000.;
        
    pIcosahedron(p);
    
    float outer, inner, hole, spike, spike2, spikes, bias1, bias2;
    
    outer = length(p) - 1.;
    inner = -(length(p) - .9);

    n = bToCn(.4,.6,.0);
    spike = fCone(p, .6, n * 0., n * 1.13) - .01;
    outer = smin(outer, spike, .1);
    
    n1 = reflect(n, triP.ca);
    spike = fCone(p, .6, n1 * 0., n1 * 1.13) - .01;
    outer = smin(outer, spike, .1);
    
    n1 = reflect(n, triP.bc);
    spike = fCone(p, .6, n1 * 0., n1 * 1.13) - .01;
    outer = smin(outer, spike, .1);
    
    n = bToCn(0,0,1);
    hole = fCone(p, .41, n * 1.3, n * 0.);
    outer = smax(outer, -hole, .05);
    
    n = bToCn(1,0,0);
    hole = fCone(p, .31, n * 1.3, n * 0.);
    outer = smax(outer, -hole, .05);
    
    d = smax(outer, inner, .05);
    
    return Model(d, vec3(0), 1.);
}


float fSpike3(vec3 p, vec3 n) {
    return fCone(p, .65, 1.7, n, 0.) - .02;
}


float fWedge(
    vec3 p,
    vec3 c1,
    vec3 c2,
    vec3 c3,
    vec3 c4,
    float thickness,
    float round
) {
    float d = 1000.;
    float part;
    vec3 n1 = cross(c1, c2);
    vec3 n2 = cross(c2, c3);
    vec3 n3 = cross(c3, c4);
    vec3 n4 = cross(c4, c1);
    part = fPlane(p, n1, thickness);
    d = part;
    part = fPlane(p, n2, thickness);
    d = smax(d, part, round);
    part = fPlane(p, n3, thickness);
    d = smax(d, part, round);
    part = fPlane(p, n4, thickness);
    d = smax(d, part, round);
    return d;
}



Model modelSave(vec3 p) {

    float part, part2, d, o;
    vec3 colour, c1, c2, c3, c4, c5, b;
    d = 1000.;
        
    pIcosahedron(p);
    
    Tri tri = Tri(pbc, pab, pca);
    vec3 rab = normalize(cross(tri.a, tri.b));
    vec3 rbc = normalize(cross(tri.b, tri.c));
    vec3 rca = normalize(cross(tri.c, tri.a));
    
    c1 = bToC(0,1,0);
    c2 = bToC(.5,.0,.5);
    c3 = bToC(1.,.0,.0);
    c4 = reflect(c2, rab);
    
    float thickness = .004;
    float round = .04;

    float hole = fWedge(p, c1, c2, c3, c4, .025, round);
      
    
    c1 = bToC(.5,.0,.5);
    c2 = bToC(0,1,0);
    c3 = reflect(c1, rbc);
    c4 = bToC(0,0,1);

    float hole2 = fWedge(p, c1, c2, c3, c4, .01, round);
    float holes = min(hole2, hole);
    
    
    float outer = length(p) - 1.;
    float inner = length(p) - .93;
    float shell = max(outer, -inner);
    
    d = shell;
    
    c1 = bToCn(1,0,0);
    float spike1 = fCone(p, 1., 1.25, c1, 0.) - .03;
    c1 = bToCn(0,1,0);
    float spike2 = fCone(p, 1.6, 1.1, c1, 0.) - .03;
    c1 = bToCn(0,0,1);
    float spike3 = fCone(p, 1.1, 1.05, c1, 0.) - .03;
    c1 = bToCn(.5,.0,.5);
    float spike4 = fCone(p, 1.15, 1.15, c1, 0.) - .03;
    
    float base = length(p) - .97;
    
    float spikes = base;
    spikes = smin(spikes, spike1, .05);
    spikes = smin(spikes, spike4, .05);
    spikes = smin(spikes, spike2, .09);
    //spikes = smin(spikes, spike3, .05);
    
    //spikes = min(spikes, length(p) - 1.);

    
    float spikesInner = spikes + 0.1;
    spikes = max(spikes, -spikesInner);
    
    d = spikes;
    
    c1 = bToCn(.0,.5,.5);
    float holee1 = fCone(p, .25, 2., -c1, -2.);
    c1 = bToCn(.5,.5,.0);
    float holee2 = fCone(p, .1, 2., -c1, -2.);
    holes = min(holee1, holee2);

    d = smax(d, -holes, .05);
    //d = min(d, holes);
    
    //d = fOpUnionRound(d, part, 0.05);
    
    //d = smax(d, -holes, 0.02);
    
    return Model(d, colour, 1.);
}


Model model9(vec3 p) {

    pIcosahedron(p);


    float part, part2, d;
    vec3 colour, c1, c2, c3, c4, c5, b, n, o;
    d = 1000.;
            
    float hole, holes;
    float spike, spikes;
    
    n = bToCn(0,0,1);
    float sharedSpike = fCone(p, .6, 1.2, n, 0.) - .02;
    
    // Shell

    float shell = length(p) - 1.;
    float shellInner = length(p) - .93;
    

    n = bToCn(1,0,0);
    hole = fCone(p, .95, 2., -n, -2.);
    holes = hole;

    n = bToCn(0,1,0);
    spike = fCone(p, 1.5, 1.1, n, 0.) - .03;
    spikes = spike;
    
    n = bToCn(0,0,1);
    spike = fCone(p, 1.5, 1.1, n, 0.) - .03;
    spike = smin(spike, sharedSpike, .1);
    spikes = spike;
    
    n = bToCn(1,0,0);
    spike = fCone(p, 1.5, 1.3, n, 0.) - .03;
    spikes = smin(spike, spikes, .1);
        

    shell = smin(shell, spikes, .01);
    shell = max(shell, -shellInner);
    shell = smax(shell, -holes, .05);    

    // Core
    
    float core = length(p) - .85;
    float coreInner = length(p) - .7;
    
    
    
    n = bToCn(1,0,0);
    spike = fCone(p, .35, 1.1, n, 0.) - .02;
    spikes = spike;
    

    core = smin(core, spikes, .3);
    
    n = bToCn(0,0,1);
    hole = fCone(p, .55, 2., -n, -2.);
    holes = hole;
    
    n = bToCn(.2,.0,.8);
    hole = fCone(p, .4, 2., -n, -2.);
    holes = min(hole, holes);
    
    c1 = bToCn(.7,.0,.3);
    c2 = bToCn(.0,.9,.1);
    c3 = reflect(c1, triP.bc);
    c4 = reflect(c2, triP.ca);
    
    holes = fWedge(p, c1, c2, c3, c4, .0, .07);
    
    core = max(core, -coreInner);
    core = smax(core, -holes, .08);
    
    
    
    n = bToCn(1,0,0);
    spike = fSpike3(p, n);
    n = reflect(n, triP.bc);
    spike = smin(spike, fSpike3(p, n), .15);
    n = reflect(n, triP.ca);
    spike = smin(spike, fSpike3(p, n), .15);
    spikes = spike;
    core = spike;
    
    n = bToCn(1,0,0);
    hole = fCone(p, .45, 2., -n, -2.);
    holes = hole;
    
    core = smax(core, -holes, .1);
    
    n = bToCn(1,0,0);
    spike = fCone(p, .35, 1.2, n, 0.) - .02;
    spikes = spike;
    
    core = min(core, spike);

    d = min(shell, core);
    //d = core;
    
    return Model(d, colour, 1.);
}



// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}

Model map( vec3 p ){
    float d = length(p) - 1.;
    Model model = modelSave(p);
    return model;
}


#pragma glslify: export(map)
