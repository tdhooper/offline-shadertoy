/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const DO_CAPTURE = false;

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
    'webgl_color_buffer_float',
  ],
  //pixelRatio: .5,
  pixelRatio: 1,
  attributes: {
    preserveDrawingBuffer: true,
  },
});
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

    node.draw = (state) => {
      // Don't mutate the original state
      state = Object.assign({}, state);

      if (node.firstPassOnly && ! firstPass) {
        return;
      }

      function attachDependencies() {
        for (let i = node.dependencies.length - 1; i >= 0; i--) {
          let dep = node.dependencies[i];
          let depBuffer = dep.node == node ? dep.node.lastBuffer : dep.node.buffer
          const texture = depBuffer.color[0]._texture;
          gl.activeTexture(gl.TEXTURE0);
          gl.bindTexture(texture.target, texture.texture);
          if (dep.filter === 'nearest') {
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
          } else if (dep.filter === 'mipmap') {
            gl.generateMipmap(gl.TEXTURE_2D);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
          } else {
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
          }
          if (dep.wrap === 'repeat') {
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
          } else if (dep.wrap === 'mirror') {
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.MIRRORED_REPEAT);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.MIRRORED_REPEAT);
          } else {
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
          }
          const s = {};
          s[dep.uniform] = depBuffer;
          Object.assign(state, s);
        }
      }

      function swapPingPong() {
        if (node.dependencies.map(dep => dep.node).indexOf(node) !== -1) {
          const lastBuffer = node.buffer;
          node.buffer = node.lastBuffer;
          node.lastBuffer = lastBuffer;
        }
      };

      function clearTarget() {
        regl.clear({
          color: [0, 0, 0, 1],
          depth: 1,
          framebuffer: node.buffer,
        });
      }
      
      function setTarget() {
        if (i !== renderNodes.length - 1) {
          Object.assign(state, {
            framebuffer: node.buffer,
          });
        }
      }

      if (state.tileIndex == 0) {
       // console.log(node.name, "scrubber: " + state.timer.elapsed, "drawindex: " + state.drawIndex + "/" + node.drawCount);

        swapPingPong();
        clearTarget();
      }

      attachDependencies();
      setTarget();

      if (DO_CAPTURE)
      {
        console.log(node.name, "scrubber: " + state.timer.elapsed, "drawindex: " + state.drawIndex + "/" + node.drawCount, "tile: " + state.tileIndex);
      }
      
      nodeCommand(state);
      gl.finish();      
    };
  });

  const fov = (defaultState && defaultState.fov) || 1 / (Math.PI / 5);

  const setupProjectionView = regl({
    uniforms: {
      projection: ({ viewportWidth, viewportHeight }) => mat4.perspective(
        [],
        1 / fov,
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
      //console.log(mouseProp[0] / context.viewportWidth, mouseProp[1] / context.viewportHeight);
      return mouseProp;
    },
  };

  const controls = defaultState && defaultState.controls
    ? createControls(defaultState.controls, uniforms) : null;

  let projectDraw;
  if (project.createDraw) {
    projectDraw = project.createDraw(uniforms, setupProjectionView);
  }

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

  const camera = createCamera(canvas, {
    position: [0, 0, 5],
    positionSpeed: 10,
    rotationSpeed: .1
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
    state.frame += state.drawIndex;
    //document.title = node.name + ' ' + state.drawIndex;
    //console.log(node.name, state.drawIndex, node.drawCount);
    setupProjectionView(state, (context) => {
      resizeBuffers(context.drawingBufferWidth, context.drawingBufferHeight);
      //resizeBuffers(context.viewportWidth, context.viewportHeight);
      drawRaymarch(state, () => {
        node.draw(state);
      });
    });
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

  const draw = (force, done) => {
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

      firstPass = false;
    } else {
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
