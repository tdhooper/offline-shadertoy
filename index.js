const fs = require('fs');
const glslify = require('glslify');
const regl = require('regl')({
  extensions: ['ext_frag_depth'],
  pixelRatio: .5,
});
const { mat4 } = require('gl-matrix');
const createMouse = require('./lib/mouse');
const createCamera = require('./lib/camera');
const StateStore = require('./lib/state-store');
const createScrubber = require('./lib/scrubber');
const Timer = require('./lib/timer');

// aura
// rhombille-triangle
// helix
// spiral-loop
// spiral-loop-2
// spiral-loop-pub
// spiral
// icosahedron-twist
// geodesic-tiling
// geodesic-tiling-free
// geodesic-twist
// helix-distance
// inverted-helix
// clifford-torus
// inverted-torus
// trefoil
// impossible-channel
// lines
// helix-wat
// peel

const frag = glslify('./projects/aura/shader.glsl');
// const defaultState = JSON.parse(fs.readFileSync('./projects/rays-and-polygons/config.json', 'utf8'));
const defaultState = null;

const setup = regl({
  uniforms: {
    projection: ({ viewportWidth, viewportHeight }) => mat4.perspective(
      [],
      Math.PI / 5,
      viewportWidth / viewportHeight,
      0.01,
      1000
    ),
    view: () => regl.prop('cameraMatrix'),
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
    iResolution: (context, props) => {
      const resolution = [context.viewportWidth, context.viewportHeight];
      return props.resolution || resolution;
    },
    iOffset: (context, props) => (props.offset || [0, 0]),
    cameraMatrix: regl.prop('cameraMatrix'),
    cameraPosition: regl.prop('cameraPosition'),
    iGlobalTime: regl.prop('timer.elapsed'),
    iTime: regl.prop('timer.elapsed'),
    iMouse: (context, props) => {
      const mouseProp = props.mouse.map(value => value * context.pixelRatio);
      mouseProp[1] = context.viewportHeight - mouseProp[1];
      // console.log(mouse[0] / context.viewportWidth);
      // console.log(mouse[1] / context.viewportHeight)
      return mouseProp;
    },
  },
});

const camera = createCamera(regl._gl.canvas, {
  position: [0, 0, 5],
});

const mouse = createMouse(regl._gl.canvas);

const timer = new Timer();
const scrubber = createScrubber(timer);

const toState = () => ({
  camera: camera.toState(),
  cameraMatrix: camera.view(),
  cameraPosition: camera.position,
  timer: timer.serialize(),
  mouse,
});

const fromState = (state) => {
  if (state.camera) {
    camera.fromState(state.camera);
    timer.fromObject(state.timer);
  }
};

const stateStore = new StateStore(toState, fromState, defaultState);

regl.frame(() => {
  camera.tick();
  scrubber.update();
  if (stateStore.update()) {
    setup(() => {
      drawRaymarch(stateStore.state);
    });
  }
});
