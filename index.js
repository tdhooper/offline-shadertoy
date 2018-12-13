const regl = require('regl')({
  extensions: ['ext_frag_depth'],
});
const { mat4 } = require('gl-matrix');
const createCube = require('primitive-cube');
const createCamera = require('./lib/camera');
const StateStore = require('./lib/state-store');


const camera = createCamera(regl._gl.canvas, {
  position: [0, 0, 5],
});

const setup = regl({
  uniforms: {
    projection: ({ viewportWidth, viewportHeight }) => mat4.perspective(
      [],
      Math.PI / 5,
      viewportWidth / viewportHeight,
      0.01,
      1000,
    ),
    view: () => camera.view(),
  },
});

// 1 x 1 x 1 Box
const mesh = createCube();


const drawPolygons = regl({
  vert: `
    precision mediump float;
    attribute vec3 position, normal;
    uniform mat4 model, view, projection;
    varying vec3 vnormal;
    void main() {
      vnormal = normal;
      gl_Position = projection * view * model * vec4(position, 1);
    }
  `,
  frag: `
    precision mediump float;
    varying vec3 vnormal;
    void main() {
      gl_FragColor = vec4(vnormal * .5 + .5, 1);
    }
  `,
  attributes: {
    position: mesh.positions,
    normal: mesh.normals,
  },
  elements: mesh.cells,
  uniforms: {
    model: mat4.identity([]),
  },
});

const drawRaymarch = regl({
  vert: `
    precision mediump float;
    attribute vec2 position;
    uniform mat4 projection;
    uniform mat4 view;
    varying vec3 eye;
    varying vec3 dir;
    varying vec3 cameraForward;

    void main() {
      vec2 vertex = 2.0 * position - 1.0;
      gl_Position = vec4(vertex, 0, 1);

      float fov = 1. / projection[1].y;
      float aspect = projection[1].y / projection[0].x;
      eye = -(view[3].xyz) * mat3(view);
      dir = vec3(vertex.x * fov * aspect, vertex.y * fov,-1.0) * mat3(view);
      cameraForward = vec3(0,0,-1) * mat3(view);
    }
  `,
  frag: `
    #extension GL_EXT_frag_depth : enable
    precision mediump float;
    uniform mat4 projection;
    varying vec3 eye;
    varying vec3 dir;
    varying vec3 cameraForward;

    float fBox(vec3 p, vec3 s) {
      p = abs(p) - s;
      return max(p.x, max(p.y, p.z));
    }

    float map(vec3 p) {
      // 1 x 1 x 1 Box
      float d = fBox(p, vec3(.5));
      d = 1e12;

      p = mod(p, 1.) - .5;
      d = min(d, length(p) - .04);
      return d;
    }

    vec3 calcNormal(vec3 p) {
      vec3 eps = vec3(.001,0,0);
      vec3 n = vec3(
        map(p + eps.xyy) - map(p - eps.xyy),
        map(p + eps.yxy) - map(p - eps.yxy),
        map(p + eps.yyx) - map(p - eps.yyx)
      );
      return normalize(n);
    }

    const float ITER = 50.;

    void main() {

      vec3 rayOrigin = eye;
      vec3 rayDirection = normalize(dir);
      vec3 rayPosition = rayOrigin;
      float rayLength = 0.;

      float distance = 0.;
      vec3 color = vec3(0);
      for (float i = 0.; i < ITER; i++) {
        rayLength += distance;
        rayPosition = rayOrigin + rayDirection * rayLength;
        distance = map(rayPosition);
        color += .05;
        if (distance < .001) {
          color *= calcNormal(rayPosition) * .5 + .5;
          break;
        }
      }

      float eyeHitZ = -rayLength * dot(rayDirection, cameraForward);

      vec3 eyeSpace = vec3(0, 0, eyeHitZ);
      float zc = ( projection * vec4(eyeSpace, 1)).z;
      float wc = ( projection * vec4(eyeSpace, 1)).w;
      float depth = (zc/wc + 1.) / 2.;

      gl_FragColor = vec4(color, 1);
      gl_FragDepthEXT = depth;
    }
  `,
  attributes: {
    position: [
      [-2, 0],
      [0, -2],
      [2, 2],
    ],
  },
  count: 3,
  uniforms: {
    model: mat4.identity([]),
  },
});

const stateStore = new StateStore(
  () => ({
    camera: camera.toState(),
  }),
  (state) => {
    if (state.camera) {
      camera.fromState(state.camera);
    }
  },
  {
    name: 'some_name',
    camera: camera.toState(),
  },
);


regl.frame(() => {
  camera.tick();
  if (stateStore.update()) {
    setup(() => {
      drawPolygons();
      drawRaymarch();
    });
  }
});
