/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const DO_CAPTURE = false;

import Stats from 'stats.js';
//import webFramesCapture from 'web-frames-capture';
import createMouse from './lib/mouse';
import { Camera } from './lib/camera';
import { FreeFlyCameraControl } from './lib/free-fly-camera-control';
import StateStore from './lib/state-store';
import createScrubber from './lib/scrubber';
import Timer from './lib/timer';
import AccumulateControl from './lib/accumulate';
import createControls from './lib/uniform-controls';
import Gizmo from './lib/gizmo/gizmo';
import GizmoRendererHooks from './lib/gizmo/gizmo-renderer-hooks';
import createRenderer from './renderer';
import defaultConfig from './default-config.json';
import { InteractionManager, InteractionObject } from './lib/interaction-manager';

var dbt = performance.now();

const canvases = document.createElement('div');
canvases.style.position = 'absolute';
canvases.style.width = window.innerWidth + 'px';
canvases.style.height = window.innerHeight + 'px';
document.body.appendChild(canvases);

export default async function main(project) {

  const canvas = document.createElement('canvas');
  canvas.style.position = 'absolute';
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
  canvas.style.width = window.innerWidth + 'px';
  canvas.style.height = window.innerHeight + 'px';
  canvases.appendChild(canvas);

  const interactionManager = new InteractionManager();
  interactionManager.attachToDOMElement(canvases);

  const canvasInteractionObject = new InteractionObject(() => {
    return 0;
  });
  interactionManager.add(canvasInteractionObject);

  const defaultState = project.config || defaultConfig;

  const controls = defaultState && defaultState.controls
  ? createControls(defaultState.controls) : null;

  const gizmoRendererHooks = new GizmoRendererHooks();
  const renderer = createRenderer(project, canvas, gizmoRendererHooks, controls);
  const gizmo = new Gizmo(gizmoRendererHooks, canvases, interactionManager);

  const stats = new Stats();
  stats.showPanel(0);
  document.body.appendChild(stats.dom);
  stats.dom.classList.add('stats');

  const mouse = createMouse(canvasInteractionObject);

  const camera = new Camera();
  camera.aspect = window.innerWidth / window.innerHeight;
  const freeFlyCameraControl = new FreeFlyCameraControl(camera, canvasInteractionObject);

  window.camera = camera;

  let debugPlane = {};

  window.dropDebugPlane = () => {
    debugPlane = {
      matrix: Array.prototype.slice.call(camera.view()),
      position: Array.prototype.slice.call(camera.position),
    };
  };

  const timer = new Timer();

  const controlsContainer = document.createElement('div');
  controlsContainer.classList.add('controls');
  controlsContainer.classList.add('scrubber-controls');
  document.body.appendChild(controlsContainer);

  const scrubber = createScrubber(controlsContainer, timer);
  const accumulateControl = new AccumulateControl(controlsContainer);

  let screenQuad = undefined;

  const toState = () => {
    const state = {
      freeFlyCameraControl: freeFlyCameraControl.toState(),
      view: camera.view(),
      cameraMatrix: camera.matrix(),
      cameraPosition: camera.position,
      cameraProjection: camera.projection(),
      timer: timer.serialize(),
      mouse: mouse.toState(),
      screenQuad,
      r: [window.innerWidth, window.innerHeight],
      debugPlane,
      frame: 0,
    };
    if (camera) {
      Object.assign(state, camera.toState());
    }
    if (accumulateControl) {
      Object.assign(state, accumulateControl.toState());
    }
    if (controls) {
      state.controls = controls.toState();
    }
    if (gizmo) {
      Object.assign(state, gizmo.toState());
    }
    return state;
  };

  const fromState = (state) => {
    if (camera) {
      camera.fromState(state);
    }
    freeFlyCameraControl.resetSimulation();
    if (state.freeFlyCameraControl) {
      freeFlyCameraControl.fromState(state.freeFlyCameraControl);
    }
    if (state.mouse) {
      mouse.fromState(state.mouse);
    }
    if (state.timer) {
      timer.fromObject(state.timer);
    }
    if (state.controls && controls) {
      controls.fromState(state.controls);
    }
    if (accumulateControl) {
      accumulateControl.fromState(state);
    }
    debugPlane = state.debugPlane;
  };

  window.resetCamera = function() {
    if (defaultState.camera) {
      camera.fromState(defaultState.camera);
    } else if (defaultState.cameraMatrix) {
      camera.fromState(defaultState.cameraMatrix);
    }
    freeFlyCameraControl.resetSimulation();
  }

  let resize = () => {
    canvases.style.width = window.innerWidth + 'px';
    canvases.style.height = window.innerHeight + 'px';
    renderer.resize(window.innerWidth, window.innerHeight);
    camera.aspect = window.innerWidth / window.innerHeight;
  }

  window.addEventListener('resize', resize);

  const stateStore = new StateStore(toState, fromState, defaultState, project.name);
  let stateChanged = true;
  let gizmoInitialised = false;


  let ignoreChanges = ['accumulate'];
  if ( ! renderer.requiredUniforms.has('iMouse')) {
    ignoreChanges.push('mouse');
  }

  (function tick(t) {

    freeFlyCameraControl.tick();
    scrubber.update();
    stateChanged = stateStore.update(ignoreChanges) || stateChanged;
    let state = stateStore.state;

    gizmo.update(state);
    gizmo.render();

    if (renderer.ready() || (! stateChanged && accumulateControl.accumulate)) {

      stats.begin();

      let drawState = Object.assign({}, state);
      Object.assign(drawState, accumulateControl.drawState(stateChanged));

      if (stateChanged || accumulateControl.accumulate) {
        stateChanged = false;
        renderer.draw(drawState);
      }

      stats.end();

      // This can be slow, so wait until we've drawn something first
      if ( ! gizmoInitialised) {
        gizmo.initialise(camera, mouse);
        gizmoInitialised = true;
      }
    }

    requestAnimationFrame(tick);

  })(performance.now());



  /*
  const captureSetup = (width, height, done) => {
    console.log('captureSetup', width, height);
    timer.pause();
    canvas.width = width;
    canvas.height = height;
    canvas.style.width = width + 'px';
    canvas.style.height = height + 'px';
    pexHelpers.poll();
    setTimeout(done, 1000);
  };

  const captureTeardown = () => {
    console.log('captureTeardown');
    screenQuad = undefined;
    // tick = ctx.frame(draw);
  };

  const captureRender = (milliseconds, quad, done) => {
    console.log('captureRender', milliseconds, quad);
    // setTimeout(function() {
      timer.set(milliseconds);
      screenQuad = quad;
      draw(false, done);
    //   setTimeout(done, 500);
    // }, 500);
  };

  // Default config used by the UI
  let captureConfig = {
    fps: 20,
    seconds: 3, // (duration)
    width: 640,
    height: 640,
    // quads: true,
    prefix: 'rtexample-',
  };

  if (defaultState && defaultState.capture) {
    captureConfig = Object.assign({}, defaultState.capture);
    captureConfig.prefix = `${defaultState.id}-`;
    if (captureConfig.scale) {
      captureConfig.width *= captureConfig.scale;
      captureConfig.height *= captureConfig.scale;
    }
  }

  webFramesCapture(
    canvas,
    captureSetup,
    captureTeardown,
    captureRender,
    captureConfig
  );
  */
};
