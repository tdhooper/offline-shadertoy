/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const EventEmitter = require('events');
const Stats = require('stats.js');
const glslify = require('glslify');
const regl = require('regl')({
  extensions: [
    'webgl_depth_texture',
    'ext_frag_depth',
    'oes_standard_derivatives',
    'oes_texture_float',
    'oes_texture_float_linear',
    'ext_shader_texture_lod',
  ],
  pixelRatio: .5,
  // pixelRatio: .15,
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
const buildRenderNodes = require('./lib/multipass');
const textureUniforms = require('./lib/textures');

var dbt = performance.now();

global.regl = regl;

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

  const events = new EventEmitter();
  function triggerDraw() {
    events.emit('draw');
  }

  const frag = shaders.main;

  const stats = new Stats();
  stats.showPanel(0);
  document.body.appendChild(stats.dom);
  stats.dom.classList.add('stats');

  const canvas = regl._gl.canvas;
  const gl = regl._gl;

  const renderNodes = buildRenderNodes(shaders);
  let firstPass = true;

  renderNodes.forEach((node, i) => {
    node.buffer = regl.framebuffer({
      width: 300,
      height: 300,
      colorType: 'float',
    });
    if (node.dependencies.map(dep => dep.node).indexOf(node) !== -1) {
      node.lastBuffer = regl.framebuffer({
        width: 300,
        height: 300,
        colorType: 'float',
      });
    }
    const nodeUniforms = {
      iResolution: (context, props) => {
        const resolution = [context.framebufferWidth, context.framebufferHeight];
        return props.resolution || resolution;
      },
    };
    node.dependencies.reduce((acc, dep) => {
      acc[dep.uniform] = regl.prop(dep.uniform);
      acc[dep.uniform + 'Size'] = (context, props) => [
        props[dep.uniform].width,
        props[dep.uniform].height,
      ];
      return acc;
    }, nodeUniforms);
    Object.assign(nodeUniforms, textureUniforms(regl, node.shader, triggerDraw));
    const nodeCommand = regl({
      frag: node.shader,
      uniforms: nodeUniforms,
      framebuffer: regl.prop('framebuffer'),
    });
    node.draw = (state) => {
      node.dependencies.forEach((dep) => {
        const texture = dep.node.buffer.color[0]._texture;
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(texture.target, texture.texture);
        // gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
        // gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
        const s = {};
        s[dep.uniform] = dep.node.buffer;
        state = Object.assign(s, state);
      });
      if (node.dependencies.map(dep => dep.node).indexOf(node) !== -1) {
        const lastBuffer = node.buffer;
        node.buffer = node.lastBuffer;
        node.lastBuffer = lastBuffer;
      }
      regl.clear({
        color: [0, 0, 0, 1],
        depth: 1,
        framebuffer: node.buffer,
      });
      if (i !== renderNodes.length - 1) {
        state = Object.assign({
          framebuffer: node.buffer,
        }, state);
      }
      nodeCommand(state);
    };
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
      view: regl.prop('view'),
    },
  });

  const m4identity = mat4.identity([]);

  const uniforms = {
    model: m4identity,
    iOffset: (context, props) => (props.offset || [0, 0]),
    cameraMatrix: regl.prop('cameraMatrix'),
    cameraPosition: regl.prop('cameraPosition'),
    debugPlaneMatrix: (context, props) => (props && props.debugPlane && props.debugPlane.matrix) || m4identity,
    debugPlanePosition: (context, props) => (props && props.debugPlane && props.debugPlane.position) || [0, 0, 1],
    iGlobalTime: (context, props) => props.timer.elapsed / 1000,
    iTime: (context, props) => props.timer.elapsed / 1000,
    firstPass: () => firstPass,
    iMouse: (context, props) => {

      const mouseProp = props.mouse.map(value => value * context.pixelRatio);
      mouseProp[1] = context.viewportHeight - mouseProp[1];
      // console.log(
      //   mouseProp[0] / context.viewportWidth,
      //   mouseProp[1] / context.viewportHeight
      // );
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

  let projectDraw;
  if (project.draw) {
    projectDraw = project.draw(drawRaymarch, renderNodes, uniforms);
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

  const draw = (force) => {
    stats.begin();
    camera.tick();
    scrubber.update();
    if (stateStore.update() || force) {
      regl.clear({
        color: [0, 0, 0, 1],
        depth: 1,
      });
      setup(stateStore.state, (context) => {
        renderNodes.forEach((node) => {
          if ( ! node.buffer) return;
          let width = context.viewportWidth;
          let height = context.viewportHeight;
          if (node.size) {
            [width, height] = node.size;
          }
          if (node.buffer.width !== width || node.buffer.height !== height) {
            node.buffer.resize(width, height);
            if (node.lastBuffer) {
              node.lastBuffer.resize(width, height);
            }
          }
        });
        if (projectDraw) {
          projectDraw(stateStore.state, context);
        } else {
          drawRaymarch(stateStore.state, () => {
            renderNodes.forEach((node) => {
              node.draw(stateStore.state);
            });
          });
        }
      });
      firstPass = false;
    }
    stats.end();
    if (dbt !== undefined) {
      console.log(performance.now() - dbt);
      dbt = undefined;
    }
  };

  let tick = regl.frame(() => draw());
  events.on('draw', () => draw(true));

  const captureSetup = (config, done) => {
    tick.cancel();
    timer.pause();
    canvas.width = config.width;
    canvas.height = config.height;
    canvas.style.width = config.width + 'px';
    canvas.style.height = config.height + 'px';
    setTimeout(done, 1000);
  };

  const captureTeardown = () => {
    screenQuad = undefined;
    tick = regl.frame(draw);
  };

  const captureRender = (milliseconds, quad, done) => {
    // setTimeout(function() {
      timer.set(milliseconds);
      screenQuad = quad;
      draw();
      done();
    //   setTimeout(done, 10);
    // }, 10);
  };

  // Default config used by the UI
  let captureConfig = {
    fps: 35,
    seconds: 1, // (duration)
    width: (640 * 3) / 2,
    height: (360 * 3) / 2,
    quads: true,
    prefix: 'plode-',
  };

  if (defaultState && defaultState.capture) {
    captureConfig = Object.assign({}, defaultState.capture);
    captureConfig.prefix = `${defaultState.id}-`;
    if (captureConfig.scale) {
      captureConfig.width *= captureConfig.scale;
      captureConfig.height *= captureConfig.scale;
    }
  }

  const webCapture = new WebCaptureClient(
    canvas,
    captureSetup,
    captureTeardown,
    captureRender,
    captureConfig
  );
};
