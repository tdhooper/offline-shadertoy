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
const createControls = require('./lib/uniform-controls');

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

const frag = glslify('./projects/rhombille-triangle/shader.glsl');
const defaultState = JSON.parse(fs.readFileSync('./projects/rhombille-triangle/config.json', 'utf8'));
// const defaultState = null;

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

const uniforms = {
  model: mat4.identity([]),
  iResolution: (context, props) => {
    const resolution = [context.viewportWidth, context.viewportHeight];
    return props.resolution || resolution;
  },
  iOffset: (context, props) => (props.offset || [0, 0]),
  cameraMatrix: regl.prop('cameraMatrix'),
  cameraPosition: regl.prop('cameraPosition'),
  iGlobalTime: (context, props) => props.timer.elapsed / 1000,
  iTime: (context, props) => props.timer.elapsed / 1000,
  iMouse: (context, props) => {
    const mouseProp = props.mouse.map(value => value * context.pixelRatio);
    mouseProp[1] = context.viewportHeight - mouseProp[1];
    // console.log(mouse[0] / context.viewportWidth);
    // console.log(mouse[1] / context.viewportHeight)
    return mouseProp;
  },
};

const controls = defaultState && defaultState.controls
  ? createControls(defaultState.controls, uniforms) : null;

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
  uniforms: uniforms,
});

const camera = createCamera(regl._gl.canvas, {
  position: [0, 0, 5],
});

const mouse = createMouse(regl._gl.canvas);

const timer = new Timer();
const scrubber = createScrubber(timer);

const toState = () => {
  const state = {
    camera: camera.toState(),
    cameraMatrix: camera.view(),
    cameraPosition: camera.position,
    timer: timer.serialize(),
    mouse,
  };
  if (controls) {
    state.controls = controls.toState();
  }
  return state;
};

const fromState = (state) => {
  if (state.camera) {
    camera.fromState(state.camera);
  }
  if (state.timer) {
    timer.fromObject(state.timer);
  }
  if (state.controls) {
    controls.fromState(state.controls);
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
