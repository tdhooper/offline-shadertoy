const Stats = require('stats.js');
const glslify = require('glslify');
const regl = require('regl')({
  extensions: ['ext_frag_depth'],
  pixelRatio: 1,
  attributes: {
    preserveDrawingBuffer: true,
  },
});
const { mat4 } = require('gl-matrix');
const WebCaptureClient = require('web-frames-capture');
const createMouse = require('./lib/mouse');
const createCamera = require('./lib/camera');
const StateStore = require('./lib/state-store');
const createScrubber = require('./lib/scrubber');
const Timer = require('./lib/timer');
const createControls = require('./lib/uniform-controls');


global.regl = regl;

module.exports = (project) => {
  const { frag } = project;
  const defaultState = project.config || null;

  const stats = new Stats();
  stats.showPanel(0);
  document.body.appendChild(stats.dom);
  stats.dom.classList.add('stats');

  const canvas = regl._gl.canvas;

  const setup = regl({
    uniforms: {
      projection: ({ viewportWidth, viewportHeight }) => mat4.perspective(
        [],
        Math.PI / 5,
        viewportWidth / viewportHeight,
        0.01,
        1000
      ),
      view: regl.prop('view'),
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
    uniforms,
  });

  const camera = createCamera(canvas, {
    position: [0, 0, 5],
  });

  const mouse = createMouse(canvas);

  const timer = new Timer();
  const scrubber = createScrubber(timer);

  const toState = () => {
    const state = {
      camera: camera.toState(),
      view: camera.view(),
      cameraMatrix: camera.view(),
      cameraPosition: camera.position,
      timer: timer.serialize(),
      mouse,
      r: [canvas.width, canvas.height],
    };
    if (controls) {
      state.controls = controls.toState();
    }
    return state;
  };

  const fromState = (state) => {
    if (state.camera) {
      camera.fromState(state.camera);
    } else if (state.cameraMatrix) {
      camera.fromState(state.cameraMatrix);
    }
    if (state.timer) {
      timer.fromObject(state.timer);
    }
    if (state.controls) {
      controls.fromState(state.controls);
    }
  };

  const stateStore = new StateStore(toState, fromState, defaultState);

  const draw = () => {
    stats.begin();
    camera.tick();
    scrubber.update();
    if (stateStore.update()) {
      regl.clear({
        color: [0, 0, 0, 1],
        depth: 1,
      });
      setup(stateStore.state, () => {
        drawRaymarch(stateStore.state);
        if (project.draw) {
          project.draw(stateStore.state);
        }
      });
    }
    stats.end();
  };

  let tick = regl.frame(draw);

  const captureSetup = (config, done) => {
    tick.cancel();
    timer.pause();
    canvas.width = config.width;
    canvas.height = config.height;
    canvas.style.width = config.width + 'px';
    canvas.style.height = config.height + 'px';
    done();
  };

  const captureTeardown = () => {
    tick = regl.frame(draw);
  };

  const captureRender = (milliseconds, done) => {
    timer.set(milliseconds);
    draw();
    done();
  };

  // Default config used by the UI
  const captureConfig = {
    fps: 100,
    seconds: 1, // (duration)
    width: 800 * 2,
    height: 800 * 2,
    prefix: 'greytrain-',
  };

  const webCapture = new WebCaptureClient(
    canvas,
    captureSetup,
    captureTeardown,
    captureRender,
    captureConfig
  );
};
