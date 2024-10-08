precision mediump float;
uniform mat4 projection;
uniform vec2 iResolution;
uniform mat4 view;
uniform float iTime;

float time;

#define PI 3.14159265

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Shortcut for 45-degrees rotation
void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

// Repeat space along one axis. Use like this to repeat along the x axis:
// <float cell = pMod1(p.x,5);> - using the return value is optional.
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

// Repeat in two dimensions
vec2 pMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    // c.x = clamp(c.x, -1., 0.);
    // c.y = clamp(c.y, -1., 0.);
    // p = mod(p + size*0.5,size) - size*0.5;
    p -= c * size;
    return c;
}

// Repeat in three dimensions
vec3 pMod3(inout vec3 p, vec3 size) {
    vec3 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5, size) - size*0.5;
    return c;
}


// Same, but mirror every second cell so all boundaries match
vec2 pModMirror2(inout vec2 p, vec2 size) {
    vec2 halfsize = size*0.5;
    vec2 c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    p *= mod(c,vec2(2))*2. - vec2(1);
    return c;
}


// Repeat around the origin by a fixed angle.
// For easier use, num of repetitions is use to specify the angle.
float pModPolar(inout vec2 p, float repetitions) {
    float angle = 2.*PI/repetitions;
    float a = atan(p.y, p.x) + angle/2.;
    float r = length(p);
    float c = floor(a/angle);
    a = mod(a,angle) - angle/2.;
    p = vec2(cos(a), sin(a))*r;
    // For an odd number of repetitions, fix cell index of the cell in -x direction
    // (cell index would be e.g. -5 and 5 in the two halves of the cell):
    if (abs(c) >= (repetitions/2.)) c = abs(c);
    return c;
}

float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin(float a, float b) {
    return smin(a, b, .0);
}

float smax(float a, float b) {
    return smax(a, b, 0.);
}

float fOpDifferenceColumns(float a, float b, float r, float n) {
    a = -a;
    float m = min(a, b);
    //avoid the expensive computation where not needed (produces discontinuity though)
    if ((a < r) && (b < r)) {
        vec2 p = vec2(a, b);
        float columnradius = r*sqrt(2.)/n/2.0;
        columnradius = r*sqrt(2.)/((n-1.)*2.+sqrt(2.));

        pR45(p);
        p.y += columnradius;
        p.x -= sqrt(2.)/2.*r;
        p.x += -columnradius*sqrt(2.)/2.;

        if (mod(n,2.) == 1.) {
            p.y += columnradius;
        }
        pMod1(p.y,columnradius*2.);

        float result = -length(p) + columnradius;
        result = max(result, p.x);
        result = min(result, a);
        return -min(result, b);
    } else {
        return -m;
    }
}


float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fBox(vec2 p, vec2 s) {
  p = abs(p) - s;
  return max(p.x, p.y);
}

// --------------------------------------------------------
// http://www.neilmendoza.com/glsl-rotation-about-an-arbitrary-axis/
// --------------------------------------------------------

mat3 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(
        oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
        oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
        oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c
    );
}

mat3 inverse(mat3 m) {
  float a00 = m[0][0], a01 = m[0][1], a02 = m[0][2];
  float a10 = m[1][0], a11 = m[1][1], a12 = m[1][2];
  float a20 = m[2][0], a21 = m[2][1], a22 = m[2][2];

  float b01 = a22 * a11 - a12 * a21;
  float b11 = -a22 * a10 + a12 * a20;
  float b21 = a21 * a10 - a11 * a20;

  float det = a00 * b01 + a01 * b11 + a02 * b21;

  return mat3(b01, (-a22 * a01 + a02 * a21), (a12 * a01 - a02 * a11),
              b11, (a22 * a00 - a02 * a20), (-a12 * a00 + a02 * a10),
              b21, (-a21 * a00 + a01 * a20), (a11 * a00 - a01 * a10)) / det;
}


// https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83

float mod289(float x){return x - floor(x * (1.0 / 289.0)) * 289.0;}
vec4 mod289(vec4 x){return x - floor(x * (1.0 / 289.0)) * 289.0;}
vec4 perm(vec4 x){return mod289(((x * 34.0) + 1.0) * x);}

float noise(vec3 p){
    vec3 a = floor(p);
    vec3 d = p - a;
    d = d * d * (3.0 - 2.0 * d);

    vec4 b = a.xxyy + vec4(0.0, 1.0, 0.0, 1.0);
    vec4 k1 = perm(b.xyxy);
    vec4 k2 = perm(k1.xyxy + b.zzww);

    vec4 c = k2 + a.zzzz;
    vec4 k3 = perm(c);
    vec4 k4 = perm(c + 1.0);

    vec4 o1 = fract(k3 * (1.0 / 41.0));
    vec4 o2 = fract(k4 * (1.0 / 41.0));

    vec4 o3 = o2 * d.z + o1 * (1.0 - d.z);
    vec2 o4 = o3.yw * d.x + o3.xz * (1.0 - d.x);

    return o4.y * d.y + o4.x * (1.0 - d.y);
}


// --------------------------------------------------------
// Spectrum colour palette
// IQ https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


mat3 cornerAxis;


void moveCam(inout vec3 p) {
  // p.z -= .2;
  // p.y += .2;
  p.x += 1.1;
  p.y += .5;
  p.z -= .05;
  // pR(p.yz, time * PI * -.5);
  // pR(p.xz, time * PI / 2.);

  p *= cornerAxis;

  // p.y += time * 2. + .5;
}

float _map(vec3 p) {
  // 1 x 1 x 1 Box
  // p /= 3.;
  // p.y /= 2.;

  // moveCam(p);

  float floor = dot(abs(p), vec3(0,-1,0)) + .5;
  float midpoint = length(p) - .1;
  float d = 1e12;

  p = mod(p, 1.) - .5;

  float th = .49;
  d = min(d, fBox(p, vec3(.55,th,th)));
  d = min(d, fBox(p, vec3(th,.55,th)));
  d = min(d, fBox(p, vec3(th,th,.55)));

  d = min(d, fBox(p, vec3(.49)));
  
  d = -d;
  // d = min(d, floor);
  d = min(d, midpoint);

  // d = 1e12;

  // d = min(d, length(p) - .04);
  return d;
}

float stair2d(vec2 p, vec2 size) {
  float g = dot(p + size.yx / 2., normalize(-size.yx));
  if (g > .001) {
    return g;
  }
  p.y -= floor((-p.x / size.x)) * size.y;
  p.x = mod(p.x, size.x) - size.x;
  return min(
    -p.y,
    max(-p.x, -p.y - size.y)
  );
}

// xyz: length, height, width
float stairPart(vec3 p, vec3 size, float steps) {
  vec2 stepSize = size.xy / vec2(steps - 1., steps);
  float d = stair2d(p.xy, stepSize);
  // st = max(st, dot(p.xy - stepSize.yx / 1.5, normalize(stepSize.yx)));
  float b = fBox(p.xz - vec2(size.z, 0), size.xz + vec2(size.z, 0));
  d = max(d, b);

  d = max(d, -p.y - size.y); // top step

  p.y -= size.y * 2.;
  float a = length(p.xy) - size.x;
  a = min(a, fBox(p.xy - vec2(0, size.x * 2.), vec2(size.x, size.x * 2.)));
  d = max(d, -a);

  d = fOpDifferenceColumns(d, -min(p.y * .5, p.x), .125, 3.);
  d = max(d, p.y);

  d = min(d, fBox(p + vec3(1.,size.y+stepSize.y/2.,-.5), vec3(.5 + size.z, stepSize.y / 2., .5 + size.z)));

  p.x -= size.x + size.z;
  p.y -= .01;
  d = min(d, length(p) - .05);
  // d = min(d, length(p.xz) - .02);

  return d;
}

float _xmap(vec3 p) {
  // p = mod(p + .5, 1.) - .5;
  // moveCam(p);
  // return dot(p, vec3(0,-1,0));
  p.y -= .9;

  moveCam(p);

  float grid = _map(p);

  pModMirror2(p.xz, vec2(2));

  // pModMirror2(p.xz, vec2(1));

  p.y -= time * .5;
  // pR(p.xz, min(c.x, c.y) * PI / 2.);
  // pR(p.xz, c.y * PI / 2.);

  float steps = 5.;
  vec3 size = vec3(.5,.25,.15);
  size.x -= size.z;
  size.y += (size.y * 2.) / (steps * 2. - 1.) / 2.;
  // float d = fBox(p.xz, vec2(.4));
  float d = 1e12;
  vec3 ppp = p;

  p.y = mod(p.y, 2.);
  vec3 pp = p;

  // return stairPart(p + vec3(0, .5, -.5), size, steps);
  float s = 1e12;

  p.y += .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  d = min(d, s);

  p = ppp;
  // d = max(d, -p.y);

  // d = min(d, grid);
  d = grid;

  return d;
}


float density;

float randomBoxes(vec3 p) {

  p += .5;
  vec3 pp = p;

  float d = 1e12;

  float nn = 3.;
  float sz = 1. / nn;
  p += sz * fract((nn + 1.) / 2.);

  vec3 c = pMod3(p, vec3(sz)) + 15.;

  float n = noise(c);
  d = fBox(p, vec3(sz / 2.) - .00);

  // also remove boxes who's center is
  // within distance of camera path

  // make all model rotation camera rotation
  // instead

  // distance from camera path is then world
  // box position fed into camera path sdf

  if (d < .005 && n < 1. - density) {
    d = -d + .0025;
  }

  p = pp;
  p = mod(p + .5, 1.) - .5;
  float hole = sz * .5 + .001;
  d = max(d, -fBox(p.xy, vec2(hole)));
  d = max(d, -fBox(p.yz, vec2(hole)));
  d = max(d, -fBox(p.zx, vec2(hole)));

  // d = min(d, length(p) - .5 * density);

  return d;
}


vec3 modelColor;

float pModPolarAngle(vec2 p, float repetitions) {
  float angle = 2.*PI/repetitions;
  float a = atan(p.y, p.x) + angle/2.;
  float c = floor(a/angle);
  return c * angle;
}

void pModPolarApply(inout vec2 p, float a) {
  pR(p, a);
  // float r = length(p);
  // p = vec2(cos(a), sin(a))*r;
}

mat3 orientConer, orientConerInv;

void calcOrientCorner() {
  float heading = PI / 4.; // y
  float attitude = atan(sqrt(1. / 2.)); // z
  float bank = atan(sqrt(1. / 3.)); // x

  float sa = sin(attitude);
  float ca = cos(attitude);
  float sb = sin(bank);
  float cb = cos(bank);
  float sh = sin(heading);
  float ch = cos(heading);

  orientConer = mat3(
    ch*ca, -ch*sa*cb + sh*sb, ch*sa*sb + sh*cb,
    sa, ca*cb, -ca*sb,
    -sh*ca, sh*sa*cb + ch*sb, -sh*sa*sb + ch*cb
  );

  orientConerInv = inverse(orientConer);
}

vec3 calcModP(vec3 p, vec3 size) {
  pR(p.yz, .0001); // fix boundry condition
  p *= orientConer;
  vec3 modP = pMod3(p, size) * size;
  modP *= orientConerInv;
  pR(modP.zy, .0001);
  return modP;
}

vec3 eye;
float anm;



float maxcomp(in vec3 p ) { return max(p.x,max(p.y,p.z));}
float sdBox( vec3 p, vec3 b )
{
    vec3  di = abs(p) - b;
    float mc = maxcomp(di);
    return min(mc,length(max(di,0.0)));
}

vec2 iBox( in vec3 ro, in vec3 rd, in vec3 rad ) 
{
    vec3 m = 1.0/rd;
    vec3 n = m*ro;
    vec3 k = abs(m)*rad;
    vec3 t1 = -n - k;
    vec3 t2 = -n + k;
	return vec2( max( max( t1.x, t1.y ), t1.z ),
	             min( min( t2.x, t2.y ), t2.z ) );
}

const mat3 ma = mat3( 0.60, 0.00,  0.80,
                      0.00, 1.00,  0.00,
                     -0.80, 0.00,  0.60 );
vec4 menger( in vec3 p )
{
    float d = sdBox(p,vec3(1.0));
    vec4 res = vec4( d, 1.0, 0.0, 0.0 );

    float s = 1.0;
    for( int m=0; m<8; m++ )
    {
	   
        vec3 a = mod( p*s, 2.0 )-1.0;
        s *= 3.0;
        vec3 r = abs(1.0 - 3.0*abs(a));
        float da = max(r.x,r.y);
        float db = max(r.y,r.z);
        float dc = max(r.z,r.x);
        float c = (min(da,min(db,dc))-1.0)/s;

        if( c>d )
        {
          d = c;
          res = vec4( d, min(res.y,0.2*da*db*dc), (1.0+float(m))/4.0, 0.0 );
        }
    }

    return res;
}


float sz = .5;
float boxSz = .5 * .45;

float map(vec3 p) {

  float scl = 1.;

  
  scl = mix(1., .6922, pow(max(0., anm - 1.), 2.));
  
  //scl = `.5;
  //scl = .25;

  p /= scl;

p.x += 1./1.5 - (1./1.5) / 3. + (1./1.5) / (3.*3.);// + (1./1.5) / (3.*3.*3.*3.);
//p.y -= 2./1.5;// + (5./1.5) / 3.;
//p.y -= .6/1.5;// + (5./1.5) / 3.;
p.y -= .65/1.5;// + (5./1.5) / 3.;

  return menger(p).x * scl;

  float sz = .5;
  float boxSz = sz / 3.;

  //p.x -= sz;
//  pR(p.yx, anm * PI * .5);

  vec3 c = pMod3(p, vec3(sz));

  float n = noise(c - 35.);
  float d = fBox(p, vec3(boxSz));


  if (c == vec3(0, 0, 0)) {
    n = 1.;
  }


  n = 1.;

  if (c.xz == vec2(0)) {
    n = 0.;
  }

  if (vmax(abs(c)) > 1.) {
    n = 0.;
  }

  if (n < .7)
  {
    d = -fBox(p, vec3(sz + boxSz) * .5);
  }

  d *= scl;

  return d;
}



const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
  vec3 eps = vec3(.0001,0,0);
  vec3 nor = vec3(0);
  float invert = 1.;
  for (int i = 0; i < NORMAL_STEPS; i++){
    nor += map(pos + eps * invert) * eps * invert;
    eps = eps.zxy;
    invert *= -1.;
  }
  return normalize(nor);
}

const float ITER = 1000.;
const float MAX_DIST = 5.;


vec3 getStereoDir() {
  vec2 p = gl_FragCoord.xy / iResolution.xy;
  float m = .25;

  m = mix(.05, .5, 1. - abs(anm - 1.));
  //m = .05;
  p = (p * 2. * m - m) * 3.142;
  p.x *= iResolution.x / iResolution.y;
  vec3 dir = vec3(
    p.x * 2.,
    dot(p, p) - 1.,
    p.y * 2.
  );
  //dir = dir.xzy;
   //pR(dir.xy, iTime * -.5);
   //pR(dir.xy, .5);
  return normalize(dir);
}



void main() {

  time = mod(iTime * .5, 1.);
  
  float tt = iTime / PI / 2.;
  tt = iTime * .25;
  tt = 2. - tt;
  //anm = smoothstep(0., 1., smoothstep(-1., 1., sin(fract(tt) * PI * 2.) * 1.25));
  anm = smoothstep(0., 1., smoothstep(0., 1., fract(tt)));
  anm += floor(tt);
  anm = mod(anm, 2.);

  // time = iTime;
  cornerAxis = rotationMatrix(normalize(vec3(1,1,-1)), time * PI * 2. / 3.);
  calcOrientCorner();

  vec2 vertex = 2.0 * (gl_FragCoord.xy / iResolution.xy) - 1.0;

  float fov = 1. / projection[1].y;
  float aspect = projection[1].y / projection[0].x;
  eye = -(view[3].xyz) * mat3(view);
  vec3 dir = vec3(vertex.x * fov * aspect, vertex.y * fov,-1.0) * mat3(view);

  dir = getStereoDir();
  // dir *= mat3(view);
  //dir.yz = dir.zy;
  //pR(dir.yz, -.4);
  eye = vec3(0,0,0);

  //eye.y = mix(2., .4, sqrt(anm));
  eye.y = 1.;
  //eye.y = mix(2., 6., sqrt(max(0., anm - 1.)));

  vec3 rayOrigin = eye;
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float distance = 0.;
  vec3 color = vec3(1);
  for (float i = 0.; i < ITER; i++) {
    rayLength += distance;
    rayPosition = rayOrigin + rayDirection * rayLength;
    // moveCam(rayPosition);
    distance = map(rayPosition);
    if (distance < .0001) {
      vec3 normal = calcNormal(rayPosition);
      color = normal * .5 + .5;
      color = vec3(1.);
      color = color * (dot(normalize(vec3(-2,1,3)), normal) * .6 + .4);
      //color = spectrum(dot(rayDirection, normal) * .75 + .75);
      break;
    }
    if (distance >= MAX_DIST) {
      break;
    }
  }
  // color *= modelColor;
  color = mix(color, vec3(.1,0,.1), pow(smoothstep(1.5, MAX_DIST, rayLength), .5));
  color = pow(color, vec3(1. / 2.2)); // Gamma


  gl_FragColor = vec4(color, 1);
}