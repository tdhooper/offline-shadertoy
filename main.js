const Stats = require('stats.js');
const glslify = require('glslify');
const regl = require('regl')({
  extensions: [
    'webgl_depth_texture',
    'ext_frag_depth',
    'oes_standard_derivatives',
  ],
  pixelRatio: .5,
  // pixelRatio: 1,
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
  const frag = project.shaders.main;
  const defaultState = project.config || null;

  const stats = new Stats();
  stats.showPanel(0);
  document.body.appendChild(stats.dom);
  stats.dom.classList.add('stats');

  const canvas = regl._gl.canvas;

  function findBuffers(shader) {
    // Reads comment after uniform, e.g.
    // uniform sampler2D iChannel0; // buffer-a.glsl, filter: linear, wrap: clamp
    const dependencies = [];
    const reUniform = /uniform sampler2D.*\/\/(.*)/g;
    const reFile = /([^\s]+)\.glsl/;
    const reFilter = /filter:\s*(\w+)/;
    const reWrap = /wrap:\s*(\w+)/;
    let match;
    do {
      match = reUniform.exec(shader);
      if (match) {
        dependencies.push({
          name: match[1].match(reFile)[1],
          filter: match[1].match(reFilter)[1],
          wrap: match[1].match(reWrap)[1],
        });
      }
    } while (match);
    return dependencies;
  }

  const nodes = {};

  Object.entries(project.shaders).forEach(([name, shader]) => {
    nodes[name] = {
      name,
      shader,
      dependencies: findBuffers(shader),
    };
  });

  // https://www.electricmonk.nl/docs/dependency_resolving_algorithm/dependency_resolving_algorithm.html
  function resolveDependency(node, resolved, unresolved) {
    unresolved.push(node);
    node.dependencies.forEach((dep) => {
      const depNode = nodes[dep.name];
      if (resolved.indexOf(depNode) === -1) {
        if (unresolved.indexOf(depNode) !== -1) {
          throw new Error(`Circular reference detected: ${node.name} -> ${depNode.name}`);
        }
        resolveDependency(depNode, resolved, unresolved);
      }
    });
    resolved.push(node);
    unresolved.push(node);
  }

  const renderOrder = [];
  resolveDependency(nodes.main, renderOrder, []);
  console.log(renderOrder);


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

  const m4identity = mat4.identity([]);

  const uniforms = {
    model: m4identity,
    iResolution: (context, props) => {
      const resolution = [context.viewportWidth, context.viewportHeight];
      return props.resolution || resolution;
    },
    iOffset: (context, props) => (props.offset || [0, 0]),
    cameraMatrix: regl.prop('cameraMatrix'),
    cameraPosition: regl.prop('cameraPosition'),
    debugPlaneMatrix: (context, props) => (props && props.debugPlane && props.debugPlane.matrix) || m4identity,
    debugPlanePosition: (context, props) => (props && props.debugPlane && props.debugPlane.position) || [0, 0, 1],
    iGlobalTime: (context, props) => props.timer.elapsed / 1000,
    iTime: (context, props) => props.timer.elapsed / 1000,
    iMouse: (context, props) => {

      const mouseProp = props.mouse.map(value => value * context.pixelRatio);
      mouseProp[1] = context.viewportHeight - mouseProp[1];
      // console.log(
      //   mouseProp[0] / context.viewportWidth,
      //   mouseProp[1] / context.viewportHeight
      // );
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

  let debugPlane = {};

  window.dropDebugPlane = () => {
    debugPlane = {
      matrix: Array.prototype.slice.call(camera.view()),
      position: Array.prototype.slice.call(camera.position),
    };
  };

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
      debugPlane,
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
    debugPlane = state.debugPlane;
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
