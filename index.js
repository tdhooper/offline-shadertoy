const glslify = require('glslify');
const regl = require('regl')({
  extensions: ['ext_frag_depth'],
});
const { mat4 } = require('gl-matrix');
const createCamera = require('./lib/camera');
const StateStore = require('./lib/state-store');


const frag = glslify('./projects/rays-and-polygons/shader.glsl');

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
      1000
    ),
    view: () => camera.view(),
  },
});

const drawRaymarch = regl({
  vert: glslify('./quad.vert'),
  frag,
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
  }
);


regl.frame(() => {
  camera.tick();
  if (stateStore.update()) {
    setup(() => {
      drawRaymarch();
    });
  }
});
