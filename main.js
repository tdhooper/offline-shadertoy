/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const createMouse = require('./lib/mouse');
const createCamera = require('./lib/camera');
const StateStore = require('./lib/state-store');
const glslify = require('glslify');
const createScrubber = require('./lib/scrubber');
const Timer = require('./lib/timer');
const AccumulateControl = require('./lib/accumulate');
const createControls = require('./lib/uniform-controls');
const buildRenderNodes = require('./lib/multipass');

const canvas = document.createElement('canvas');
document.body.appendChild(canvas);



module.exports = (project) => {
  const defaultState = project.config || null;
  const shaders = Object.assign({}, project.shaders);

  if (shaders.common) {
    Object.entries(shaders).forEach(([name, shader]) => {
      if (name !== 'common') {
        shaders[name] = `${shaders.common}\n\n${shader}`;
      }
    });
  }

  const vertexShader = glslify('./quad.vert');

  const renderNodes = buildRenderNodes(shaders);

  // const controlUniforms = {};

  // const controls = defaultState && defaultState.controls
  //   ? createControls(defaultState.controls, controlUniforms) : null;


  const camera = createCamera(canvas, {
    position: [0, 0, 5],
  });

  window.camera = camera;

  let debugPlane = {};

  window.dropDebugPlane = () => {
    debugPlane = {
      matrix: Array.prototype.slice.call(camera.view()),
      position: Array.prototype.slice.call(camera.position),
    };
  };

  const mouse = createMouse(canvas);

  const timer = new Timer();

  const controlsContainer = document.createElement('div');
  controlsContainer.classList.add('controls');
  document.body.appendChild(controlsContainer);

  const scrubber = createScrubber(controlsContainer, timer);
  const accumulateControl = new AccumulateControl(controlsContainer);

  let screenQuad = undefined;

  const toState = () => {
    const state = {
      camera: camera.toState(),
      view: camera.view(),
      cameraMatrix: camera.view(),
      cameraPosition: camera.position,
      timer: timer.serialize(),
      accumulateControl: accumulateControl.serialize(),
      mouse,
      screenQuad,
      r: [canvas.width, canvas.height],
      debugPlane,
    };
    // if (controls) {
    //   state.controls = controls.toState();
    // }
    return state;
  };

  const fromState = (state) => {
    if (state.camera) {
      camera.fromState(state.camera);
    } else if (state.cameraMatrix) {
      camera.fromState(state.cameraMatrix);
    }
    if (state.mouse) {
      mouse[0] = state.mouse[0];
      mouse[1] = state.mouse[1];
      mouse[2] = state.mouse[2];
      mouse[3] = state.mouse[3];
    }
    if (state.timer) {
      timer.fromObject(state.timer);
    }
    // if (state.controls) {
    //   controls.fromState(state.controls);
    // }
    if (state.accumulateControl) {
      accumulateControl.fromObject(state.accumulateControl);
    }
    debugPlane = state.debugPlane;
  };

  window.resetCamera = function() {
    if (defaultState.camera) {
      camera.fromState(defaultState.camera);
    } else if (state.cameraMatrix) {
      camera.fromState(defaultState.cameraMatrix);
    }
  }

  const stateStore = new StateStore(toState, fromState, defaultState);

  let frame = 0;

  const fov = (defaultState && defaultState.fov) || 1 / (Math.PI / 5);


  // INIT WORKER
  var offscreen = canvas.transferControlToOffscreen();
  var worker = new Worker('render.js');
  worker.postMessage({
    canvas: offscreen,
    shaders,
    vertexShader,
    renderNodes,
    fov
  }, [offscreen]);


  const draw = (force) => {
    camera.tick();
    scrubber.update();
    let stateChanged = stateStore.update(['accumulateControl']);

    if (stateChanged || force || accumulateControl.accumulate) {

      let state = Object.assign(accumulateControl.drawState(stateChanged, force), stateStore.state);
      state.frame = frame++;

      // CALL WORKER
      worker.postMessage({
        draw: true,
        state,
      });
    }
  };

  //let tick = regl.frame(() => draw());
  //events.on('draw', () => draw(true));
  //let tick;

  draw();

};
