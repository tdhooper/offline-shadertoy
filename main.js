const Stats = require('stats.js');
const glslify = require('glslify');
const regl = require('regl')({
  extensions: [
    'webgl_depth_texture',
    'ext_frag_depth',
    'oes_standard_derivatives',
  ],
  // pixelRatio: .15,
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

var dbt = performance.now();

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


  const buffer = regl.framebuffer({
    width: 1024,
    height: 1024,
    depthTexture: true,
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
      console.log(
        mouseProp.x / context.viewportWidth,
        mouseProp.y / context.viewportHeight
      );
      return mouseProp;
    },
    uDepth: buffer.depthStencil,
    uSource: buffer,
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
    viewport: {
      x: function(context, props) {
        var s = context.viewportWidth;
        if (props.screenQuad !== undefined) {
          return props.screenQuad % 2 === 1 ? -s : 0;
        }
        return 0;
      },
      y: function(context, props) {
        var s = context.viewportHeight;
        if (props.screenQuad !== undefined) {
          return props.screenQuad < 2 ? -s : 0;
        }
        return 0;
      },
      width: function(context, props) {
        var s = context.viewportWidth;
        if (props.screenQuad !== undefined) {
          return s * 2;
        }
        return s;
      },
      height: function(context, props) {
        var s = context.viewportHeight;
        if (props.screenQuad !== undefined) {
          return s * 2;
        }
        return s;
      },
    }
  });

  if (project.draw) {
    var projectDraw = project.draw(uniforms);
  }

  const camera = createCamera(canvas, {
    position: [0, 0, 5],
  });

  const mouse = createMouse(canvas);

  const timer = new Timer();
  const scrubber = createScrubber(timer);
  let screenQuad = undefined;

  const toState = () => {
    const state = {
      camera: camera.toState(),
      view: camera.view(),
      cameraMatrix: camera.view(),
      cameraPosition: camera.position,
      timer: timer.serialize(),
      mouse,
      screenQuad,
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
    if (state.mouse) {
      mouse[0] = state.mouse[0];
      mouse[1] = state.mouse[1];
      mouse[2] = state.mouse[2];
      mouse[3] = state.mouse[3];
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
      regl.clear({
        color: [0, 0, 0, 1],
        depth: 1,
        framebuffer: buffer,
      });
      setup(stateStore.state, (context) => {
        if (
          buffer.width !== context.viewportWidth
          || buffer.height !== context.viewportHeight
        ) {
          buffer.resize(context.viewportWidth, context.viewportHeight);
        }
        if (projectDraw) {
          buffer.use(function() {
            projectDraw(stateStore.state);
          });
        }
        drawRaymarch(stateStore.state);
      });
    }
    stats.end();
    if (dbt !== undefined) {
      console.log(performance.now() - dbt);
      dbt = undefined;
    }
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
    screenQuad = undefined;
    tick = regl.frame(draw);
  };

  const captureRender = (milliseconds, quad, done) => {
    setTimeout(function() {
      timer.set(milliseconds);
      screenQuad = quad;
      draw();
      setTimeout(done, 10);
    }, 10);
  };

  // Default config used by the UI
  const captureConfig = {
    fps: 35,
    seconds: 1, // (duration)
    width: (640 * 3) / 2,
    height: (360 * 3) / 2,
    quads: true,
    prefix: 'plode-',
  };

  const webCapture = new WebCaptureClient(
    canvas,
    captureSetup,
    captureTeardown,
    captureRender,
    captureConfig
  );
};
