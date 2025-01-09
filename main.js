/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const DO_CAPTURE = false;

import Stats from 'stats.js';
//import webFramesCapture from 'web-frames-capture';
import createMouse from './lib/mouse';
import createCamera from './lib/camera';
import StateStore from './lib/state-store';
import createScrubber from './lib/scrubber';
import Timer from './lib/timer';
import AccumulateControl from './lib/accumulate';
import createControls from './lib/uniform-controls';
//import Gizmo from './lib/gizmo/gizmo';
import defaultConfig from './default-config.json';
import createRenderer from './renderer';

var dbt = performance.now();

const canvases = document.createElement('div');
canvases.style.position = 'absolute';
canvases.style.width = window.innerWidth + 'px';
canvases.style.height = window.innerHeight + 'px';
document.body.appendChild(canvases);



export default function main(project) {
  const defaultState = project.config || defaultConfig;

  const canvas = document.createElement('canvas');
  canvas.style.position = 'absolute';
  canvas.style.width = window.innerWidth + 'px';
  canvas.style.height = window.innerHeight + 'px';
  canvases.appendChild(canvas);

  //const bitmapContext = canvas.getContext("bitmaprenderer");

//  let gizmo = new Gizmo();

//  gizmo.preprocessShaders(Object.values(shaders));

  const stats = new Stats();
  stats.showPanel(0);
  document.body.appendChild(stats.dom);
  stats.dom.classList.add('stats');


  //const renderNodes = buildRenderNodes(shaders);

  const fov = (defaultState && defaultState.fov) || 1 / (Math.PI / 5);

  const mouse = createMouse(canvases);

  const camera = createCamera(mouse, {
    position: [0, 0, 5],
    positionSpeed: 10,
    rotationSpeed: .1
  });

  window.camera = camera;

  const controls = defaultState && defaultState.controls
    ? createControls(defaultState.controls, uniforms) : null;

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
      camera: camera.toState(),
      view: camera.view(),
      cameraMatrix: camera.view(),
      cameraPosition: camera.position,
      cameraFov: fov,
      timer: timer.serialize(),
      accumulateControl: accumulateControl.serialize(),
      mouse: mouse.toState(),
      screenQuad,
      r: [canvas.width, canvas.height],
      debugPlane,
    };
    if (controls) {
      state.controls = controls.toState();
    }
    //if (gizmo) {
    //  Object.assign(state, gizmo.toState());
    //}
    return state;
  };

  const fromState = (state) => {
    if (state.camera) {
      camera.fromState(state.camera);
    } else if (state.cameraMatrix) {
      camera.fromState(state.cameraMatrix);
    }
    if (state.mouse) {
      mouse.fromState(state.mouse);
    }
    if (state.timer) {
      timer.fromObject(state.timer);
    }
    if (state.controls) {
      controls.fromState(state.controls);
    }
    if (state.accumulateControl) {
      accumulateControl.fromObject(state.accumulateControl);
    }
    debugPlane = state.debugPlane;
  };

  window.resetCamera = function() {
    if (defaultState.camera) {
      camera.fromState(defaultState.camera);
    } else if (defaultState.cameraMatrix) {
      camera.fromState(defaultState.cameraMatrix);
    }
  }

  const stateStore = new StateStore(toState, fromState, defaultState, project.name);

  let frame = 0;

  const renderer = createRenderer(project);

  
  let resize = () => {
    canvases.style.width = window.innerWidth + 'px';
    canvases.style.height = window.innerHeight + 'px';
    renderer.resize(window.innerWidth, window.innerHeight);
  }

  window.addEventListener('resize', resize);


  let draw = (force, done) => {
    camera.tick();
    scrubber.update();

    let stateChanged = stateStore.update();
    let state = Object.assign(accumulateControl.drawState(stateChanged, force), stateStore.state);

    //console.log(stateChanged);
    if (stateChanged || force || accumulateControl.accumulate) {

      state.frame = frame++;

      // gizmo.update(state);
      // gizmo.render();

      renderer.draw(state, done);

    } else {
      // gizmo.render();
      if (done) { done(); }
    }

    // this can be slow, so wait until we've drawn something first
    // if (!gizmo.initialised) {
    //   gizmo.initialise(camera, mouse, renderNodes, uniforms);
    // }   
  };




  // let i = -1;
  // (function next () {
  //   i += 1;
  //   if (i >= node.drawCount) {
  //     done();
  //     return;
  //   }

  //   attachDependencies();
  //   swapPingPong();
  //   clearTarget();
  //   setTarget();
  //   Object.assign(state, {drawIndex: i});
  //   repeatTile(state);
  //   state.frame += 1;
  //   gl.finish();
    
  //   next();          
  // })();

  if ( ! DO_CAPTURE)
  {
    (function tick(t) {
      //console.log(t);
      stats.begin();
      draw(false, () => {
        stats.end();
        if (dbt !== undefined) {
          console.log('dbt', performance.now() - dbt);
          dbt = undefined;
        }
        requestAnimationFrame(tick);
      });
    })(performance.now());
  }

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
