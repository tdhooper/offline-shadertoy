/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const createRegl = require('regl');
const { mat4 } = require('gl-matrix');
const textureUniforms = require('./lib/textures');

let draw;

const init = (
  canvas,
  shaders,
  vertexShader,
  renderNodes,
  fov
) => {

  const regl = createRegl({
    canvas: canvas,
    extensions: [
      'webgl_depth_texture',
      'ext_frag_depth',
      'oes_standard_derivatives',
      'oes_texture_float',
      'oes_texture_float_linear',
      'ext_shader_texture_lod',
    ],
    pixelRatio: .25,
    //pixelRatio: 1,
    attributes: {
      preserveDrawingBuffer: true,
    },
  });

  global.regl = regl;

  const frag = shaders.main;
  const gl = regl._gl;

  let firstPass = true;

  function triggerDraw() {
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
          const w = Math.ceil(context.viewportWidth / node.tile);
          const h = Math.ceil(context.viewportHeight / node.tile);
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
        node.dependencies.forEach((dep) => {
          const texture = dep.node.buffer.color[0]._texture;
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
          s[dep.uniform] = dep.node.buffer;
          Object.assign(state, s);
        });
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

      function repeatTile(state) {
        if (node.tile) {
          for(let i = 0; i < node.tile * node.tile; i++) {
            Object.assign(state, {tileIndex: i});
            nodeCommand(state);
            gl.finish();
          }
        } else {
          nodeCommand(state)
        }  
      } 

      if (node.drawCount && ! state.isAccumulationDraw) {
      //if (node.drawCount) {
        for(let i = 0; i < node.drawCount; i++) {
          attachDependencies();
          swapPingPong();
          clearTarget();
          setTarget();
          Object.assign(state, {drawIndex: i});
          repeatTile(state);
          state.frame += 1;
          gl.finish();
        }
      } else {
        attachDependencies();
        swapPingPong();
        clearTarget();
        setTarget();
        repeatTile(state)
      }  
    };
  });

  const setup = regl({
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

  //Object.assign(uniforms, controlUniforms);

  const drawRaymarch = regl({
    vert: vertexShader,
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
  // if (project.draw) {
  //   projectDraw = project.draw(drawRaymarch, renderNodes, uniforms);
  // }

  draw = (state) => {


    regl.clear({
      color: [0, 0, 0, 1],
      depth: 1,
    });

    setup(state, (context) => {
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
        projectDraw(state, context);
      } else {
        drawRaymarch(state, () => {
          renderNodes.forEach((node) => {
            node.draw(state);
          });
        });
      }
    });
    firstPass = false;
  };
};


onmessage = function (e) {
  if (e.data.canvas) {
    init(
      e.data.canvas,
      e.data.shaders,
      e.data.vertexShader,
      e.data.renderNodes,
      e.data.fov
    );
  }
  if (e.data.draw) {
    draw(
      e.data.state
    );
  }
};
