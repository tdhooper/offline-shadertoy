/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const DO_CAPTURE = false;

const EventEmitter = require('events');
const Stats = require('stats.js');
const glslify = require('glslify');
const createRegl = require('regl');
const { mat4 } = require('gl-matrix');
const webFramesCapture = require('web-frames-capture');
const createMouse = require('./lib/mouse');
const createCamera = require('./lib/camera');
const StateStore = require('./lib/state-store');
const createScrubber = require('./lib/scrubber');
const Timer = require('./lib/timer');
const AccumulateControl = require('./lib/accumulate');
const createControls = require('./lib/uniform-controls');
const buildRenderNodes = require('./lib/multipass');
const bindBuffer = require('./lib/bind-buffer');
const textureUniforms = require('./lib/textures');
const Gizmo = require('./lib/gizmo');

var dbt = performance.now();

const canvases = document.createElement('div');
canvases.style.position = 'absolute';
canvases.style.width = window.innerWidth + 'px';
canvases.style.height = window.innerHeight + 'px';
window.addEventListener('resize', () => {
  canvases.style.width = window.innerWidth + 'px';
  canvases.style.height = window.innerHeight + 'px';
});
document.body.appendChild(canvases);

const regl = createRegl({
  container: canvases,
  extensions: [
    'webgl_depth_texture',
    'ext_frag_depth',
    'oes_standard_derivatives',
    'oes_texture_float',
    'oes_texture_float_linear',
    'ext_shader_texture_lod',
    'webgl_color_buffer_float',
  ],
  pixelRatio: .5,
  //pixelRatio: 1,
  attributes: {
    preserveDrawingBuffer: true,
  },
})
global.regl = regl;

module.exports = (project) => {
  const defaultState = project.config || null;
  const shaders = Object.assign({}, project.shaders);

  if (shaders.common) {
    Object.entries(shaders).forEach(([name, shader]) => {
      if (name !== 'common') {
        shader.glsl = `${shaders.common}\n\n${shader.glsl}`;
      }
    });
  }

  let gizmo = new Gizmo();
  
  gizmo.preprocessShaders(Object.values(shaders));

  const events = new EventEmitter();
  function triggerDraw() {
    events.emit('draw');
  }

  const frag = shaders.main.glsl;

  const stats = new Stats();
  stats.showPanel(0);
  document.body.appendChild(stats.dom);
  stats.dom.classList.add('stats');

  const canvas = regl._gl.canvas;
  const gl = regl._gl;
  canvas.style.position = 'absolute';

  const renderNodes = buildRenderNodes(shaders);
  let firstPass = true;

  function attachDependencies(node, state) {
    for (let i = node.dependencies.length - 1; i >= 0; i--) {
      let dep = node.dependencies[i];
      let depBuffer = dep.node == node ? dep.node.lastBuffer : dep.node.buffer;
      const texture = depBuffer.color[0]._texture;
      bindBuffer(texture, dep.filter, dep.wrap);
      const s = {};
      s[dep.uniform] = depBuffer;
      Object.assign(state, s);
    }
  }

  function swapPingPong(node) {
    if (node.dependencies.map(dep => dep.node).indexOf(node) !== -1) {
      const lastBuffer = node.buffer;
      node.buffer = node.lastBuffer;
      node.lastBuffer = lastBuffer;
    }
  };

  function clearTarget(node) {
    regl.clear({
      color: [0, 0, 0, 1],
      depth: 1,
      framebuffer: node.buffer,
    });
  }

  function setTarget(node, state) {
    if ( ! node.final) {
      Object.assign(state, {
        framebuffer: node.buffer,
      });
    }
  }

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
      drawIndex: (context, props) => props.drawIndex,
      iFrame: regl.prop('frame'),
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
      scissor: {
        enable: Boolean(node.tile),
        box: (context, props) => {
          if ( ! node.tile) {
            return {};
          }
          const i = props.tileIndex;
          const w = Math.ceil(context.framebufferWidth / node.tile);
          const h = Math.ceil(context.framebufferHeight / node.tile);
          const x = i % node.tile;
          const y = Math.floor(i / node.tile);
          return {
            x: x * w,
            y: y * h,
            width: w,
            height: h
          };
        },
      },
    });

    node.draw = (state, body) => {
      setupProjectionView(state, () => {
        drawRaymarch(state, () => {
          attachDependencies(node, state);
          if (DO_CAPTURE)
          {
            console.log(node.name, "scrubber: " + state.timer.elapsed, "drawindex: " + state.drawIndex + "/" + node.drawCount, "tile: " + state.tileIndex);
          }
          nodeCommand(state, body);
        });
      });
    }
  });

  const fov = (defaultState && defaultState.fov) || 1 / (Math.PI / 5);

  const setupContext = regl({});

  const setupProjectionView = regl({
    uniforms: {
      projection: (context, props) => mat4.perspective(
        [],
        1 / props.cameraFov,
        context.viewportWidth / context.viewportHeight,
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
      //console.log(mouseProp[0] / context.viewportWidth, mouseProp[1] / context.viewportHeight);
      return mouseProp;
    },
  };

  const mouse = createMouse(canvases);

  const camera = createCamera(mouse, {
    position: [0, 0, 5],
    positionSpeed: 10,
    rotationSpeed: .1
  });

  window.camera = camera;


  gizmo.initialise(camera, mouse, renderNodes, uniforms);

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
        var s = context.drawingBufferWidth;
        if (props.screenQuad !== undefined) {
          return props.screenQuad % 2 === 1 ? -s : 0;
        }
        return 0;
      },
      y: function(context, props) {
        var s = context.drawingBufferHeight;
        if (props.screenQuad !== undefined) {
          return props.screenQuad < 2 ? -s : 0;
        }
        return 0;
      },
      width: function(context, props) {
        var s = context.drawingBufferWidth;
        if (props.screenQuad !== undefined) {
          return s * 2;
        }
        return s;
      },
      height: function(context, props) {
        var s = context.drawingBufferHeight;
        if (props.screenQuad !== undefined) {
          return s * 2;
        }
        return s;
      },
    }
  });

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
    if (gizmo) {
      Object.assign(state, gizmo.toState());
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
    } else if (state.cameraMatrix) {
      camera.fromState(defaultState.cameraMatrix);
    }
  }

  const stateStore = new StateStore(toState, fromState, defaultState);

  let frame = 0;

  const resizeBuffers = (viewportWidth, viewportHeight) => {
    renderNodes.forEach((node) => {
      if ( ! node.buffer) return;
      let width = viewportWidth;
      let height = viewportHeight;
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
  }

  const drawNode = (node, state) => {

    if (node.firstPassOnly && ! firstPass) {
      return;
    }

    // Don't mutate the original state
    state = Object.assign({}, state);

    if (state.tileIndex == 0) {
      // console.log(node.name, "scrubber: " + state.timer.elapsed, "drawindex: " + state.drawIndex + "/" + node.drawCount);
      swapPingPong(node);
      clearTarget(node);
    }

    setTarget(node, state);

    //document.title = node.name + ' ' + state.drawIndex;
    //console.log(node.name, state.drawIndex, node.drawCount);
    setupContext(state, (context) => {
      resizeBuffers(context.drawingBufferWidth, context.drawingBufferHeight);
      // call gimo.setUniforms here?
      node.draw(state);
    });

    gl.finish();
  };

  const drawNodes = (
    state,
    done,
    nodeIndex,
    nodeDrawIndex,
    tileIndex
  ) => {
    if (nodeIndex >= renderNodes.length) {
      done();
      return;
    }

    let node = renderNodes[nodeIndex];

    let initialDrawIndex = state.drawIndex;
    if (node.drawCount) {
      state.drawIndex = initialDrawIndex * node.drawCount + nodeDrawIndex;
    }
    state.tileIndex = tileIndex;
    state.frame += state.drawIndex;

    drawNode(node, state);

    state.drawIndex = initialDrawIndex;

    if ( ! node.tile) {
      nodeDrawIndex += 1;
    } else {
      tileIndex += 1;
      if (tileIndex >= node.tile * node.tile) {
        tileIndex = 0;
        nodeDrawIndex += 1;
      }
    }
    if ( ! node.drawCount || nodeDrawIndex >= node.drawCount) {
      //console.clear();
      nodeIndex += 1;
      nodeDrawIndex = 0;
      tileIndex = 0;
    }

    if (DO_CAPTURE) {
      requestAnimationFrame(() => {
        drawNodes(state, done, nodeIndex, nodeDrawIndex, tileIndex);
      });
    } else {
      drawNodes(state, done, nodeIndex, nodeDrawIndex, tileIndex);
    }
  }

  let draw;
  let projectDraw;
  let projectDrawRequestDraw = (force, done) => {
    if (draw) {
      draw(force, done);
    }
  };
  
  if (project.createDraw) {
    projectDraw = project.createDraw(
      uniforms,
      setupProjectionView,
      projectDrawRequestDraw,
      camera,
      project,
    );
  }


  draw = (force, done) => {
    camera.tick();
    scrubber.update();
    let stateChanged = stateStore.update(['accumulateControl']);
    if (stateChanged || force || accumulateControl.accumulate) {
      regl.clear({
        color: [0, 0, 0, 1],
        depth: 1,
      });

      let state = Object.assign(accumulateControl.drawState(stateChanged, force), stateStore.state);
      state.frame = frame++;

      if (projectDraw) {
        projectDraw(state, () => {
          drawNodes(state, done, 0, 0, 0);
        })
      } else {
        drawNodes(state, done, 0, 0, 0);
      }

      gizmo.update(state);
      gizmo.render();

      firstPass = false;
    } else {
      gizmo.render();
      done();
    }
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
          //console.log('dbt', performance.now() - dbt);
          dbt = undefined;
        }
        requestAnimationFrame(tick);
      });
      regl.poll();
    })(performance.now());
  }

  //let tick = regl.frame(() => draw());
  //events.on('draw', () => draw(true));
  

  const captureSetup = (width, height, done) => {
    console.log('captureSetup', width, height);
    timer.pause();
    canvas.width = width;
    canvas.height = height;
    canvas.style.width = width + 'px';
    canvas.style.height = height + 'px';
    regl.poll();
    setTimeout(done, 1000);
  };

  const captureTeardown = () => {
    console.log('captureTeardown');
    screenQuad = undefined;
    // tick = regl.frame(draw);
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
};
