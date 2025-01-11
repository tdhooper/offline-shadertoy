/* eslint no-param-reassign: ["error", { "props": false }] */
/* eslint space-unary-ops: [2, { "overrides": {"!": true} }] */

const DO_CAPTURE = false;

import * as pexHelpers from './lib/pex-helpers';
import { mat4 } from 'gl-matrix';
import buildRenderNodes from './lib/multipass';
import textureUniforms from './lib/textures';
import createContext from 'pex-context';

export default function createRenderer(project, canvas, gizmoRendererHooks, controls) {
  const shaders = Object.assign({}, project.shaders);

  if (shaders.common) {
    Object.entries(shaders).forEach(([name, shader]) => {
      if (name !== 'common') {
        shader.glsl = `${shaders.common.glsl}\n\n${shader.glsl}`;
      }
    });
  }

  gizmoRendererHooks.preprocessShaders(Object.values(shaders));

  let webgl2 = Object.values(shaders).some(shader => shader.glsl.indexOf('#version 300 es') !== -1);

  const ctx = createContext({
    type: webgl2 ? 'webgl2' : 'webgl',
    //pixelRatio: .5,
    //pixelRatio: 1,
    width: canvas.width,
    height: canvas.height,
    canvas: canvas,
    element: null
  });
  
  ctx.gl.getExtension("EXT_frag_depth");

  //ctx.debug(true);

  //ctx.set({pixelRatio: .5});

  self.ctx = ctx;

  const renderNodes = buildRenderNodes(shaders);
  let firstPass = true;

  function attachDependencies(node, state) {
    for (let i = node.dependencies.length - 1; i >= 0; i--) {
      let dep = node.dependencies[i];
      let depBuffer = dep.node == node ? dep.node.lastBuffer : dep.node.buffer;
      let texture = pexHelpers.passTex(depBuffer);
      let filter = pexHelpers.filterConst(dep.filter);
      //let wrap = pexHelpers.wrapConst(dep.wrap);
      ctx.update(texture, {
        min: filter,
        mag: filter,
        //wrap: ctx.gl.REPEAT,
      });
      const s = {};
      s[dep.uniform] = texture;
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

  function setTarget(node, state) {
    if ( ! node.final) {
      Object.assign(state, {
        pass: node.buffer,
      });
    }
  }

  const m4identity = mat4.identity([]);

  const screenPass = ctx.pass({});

  const uniforms = {
    model: m4identity,
    iOffset: (context, props) => (props.offset || [0, 0]),
    cameraMatrix: pexHelpers.cmdProp('cameraMatrix'),
    cameraPosition: pexHelpers.cmdProp('cameraPosition'),
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
    projection: (context, props) => mat4.perspective(
      [],
      1 / props.cameraFov,
      context.viewportWidth / context.viewportHeight,
      0.01,
      1000
    ),
    view: pexHelpers.cmdProp('view'),
  };

  gizmoRendererHooks.preprocessUniforms(uniforms);
  gizmoRendererHooks.preprocessRenderNodes(renderNodes);

  if (controls) controls.addUniforms(uniforms);

  renderNodes.forEach((node, i) => {
    node.buffer = pexHelpers.createPass({
      width: ctx.gl.drawingBufferWidth,
      height: ctx.gl.drawingBufferHeight,
      pixelFormat: ctx.PixelFormat.RGBA32F,
    });
    if (node.dependencies.map(dep => dep.node).indexOf(node) !== -1) {
      node.lastBuffer = pexHelpers.createPass({
        width: ctx.gl.drawingBufferWidth,
        height: ctx.gl.drawingBufferHeight,
        pixelFormat: ctx.PixelFormat.RGBA32F,
      });
    }
    const nodeUniforms = {
      iResolution: (context, props) => {
        const resolution = [context.framebufferWidth, context.framebufferHeight];
        return props.resolution || resolution;
      },
      drawIndex: (context, props) => props.accumulate.drawIndex,
      iFrame: pexHelpers.cmdProp('frame'),
    };
    node.dependencies.reduce((acc, dep) => {
      acc[dep.uniform] = pexHelpers.cmdProp(dep.uniform);
      acc[dep.uniform + 'Size'] = (context, props) => [
        props[dep.uniform].width,
        props[dep.uniform].height,
      ];
      return acc;
    }, nodeUniforms);

    // TODO: Trigger draw on load
    Object.assign(nodeUniforms, textureUniforms(ctx, node.shader, () => {}));

    const nodeCommand = {
      pipeline: ctx.pipeline({
        vert: node.vert,
        frag: node.shader,
        depthTest: true,
      }),
      pass: (context, props) => {
        if (props.pass) {
          return props.pass;
        }
        return screenPass;
      },
      attributes: {
        position: ctx.vertexBuffer([
          [-2, 0],
          [0, -2],
          [2, 2],
        ]),
      },
      count: 3,
      uniforms: nodeUniforms,
      scissor: (context, props) => {
        if ( ! node.tile) {
          return null;
        }
        const i = props.tileIndex;
        const w = Math.ceil(context.framebufferWidth / node.tile);
        const h = Math.ceil(context.framebufferHeight / node.tile);
        const x = i % node.tile;
        const y = Math.floor(i / node.tile);
        return [x * w, y * h, w, h]
      },
      viewport: (context, props) => {
        let x = 0;
        let y = 0;
        let width = context.drawingBufferWidth;
        let height = context.drawingBufferHeight;
        if (props.screenQuad !== undefined) {
          x = props.screenQuad % 2 === 1 ? -context.drawingBufferWidth : 0;
          y = props.screenQuad < 2 ? -context.drawingBufferHeight : 0;
          width = context.drawingBufferWidth * 2;
          height = context.drawingBufferHeight * 2;
        }
        return [x, y, width, height];
      },
    };

    node.draw = (state, partialCmd) => {
      attachDependencies(node, state);
      if (DO_CAPTURE)
      {
        console.log(node.name, "scrubber: " + state.timer.elapsed, "drawindex: " + state.accumulate.drawIndex + "/" + node.drawCount, "tile: " + state.tileIndex);
      }
      let cmds = [nodeCommand, { uniforms }];
      if (partialCmd) {
        cmds.push(partialCmd);
      }
      let cmd = pexHelpers.evalCmds(cmds, state);
      ctx.submit(cmd);
    }
  });

  const resizeBuffers = (viewportWidth, viewportHeight) => {
    renderNodes.forEach((node) => {
      if ( ! node.buffer) return;
      let width = viewportWidth;
      let height = viewportHeight;
      if (node.size) {
        [width, height] = node.size;
      }
      if (pexHelpers.passTex(node.buffer).width !== width || pexHelpers.passTex(node.buffer).height !== height) {
        pexHelpers.resizePass(node.buffer, width, height);
        if (node.lastBuffer) {
          pexHelpers.resizePass(node.lastBuffer, width, height);
        }
      }
    });
  }

  const drawNode = (node, state) => {
    if (node.firstPassOnly && ! firstPass) {
      return;
    }
    if (state.tileIndex == 0) {
      swapPingPong(node);
    }
    // Don't mutate the original state TOOD: test this is necessary
    state = Object.assign({}, state);
    setTarget(node, state);

    node.draw(state);
  };

  const drawNodes = (
    state,
    nodeIndex,
    nodeDrawIndex,
    tileIndex
  ) => {

    // Don't mutate the original state TOOD: test this is necessary
    state = Object.assign({}, state);

    if (nodeIndex == 0 && tileIndex == 0 && nodeDrawIndex == 0) {
      resizeBuffers(ctx.gl.drawingBufferWidth, ctx.gl.drawingBufferHeight);
    }

    if (nodeIndex >= renderNodes.length) {
      return;
    }

    let node = renderNodes[nodeIndex];

    let initialDrawIndex = state.accumulate.drawIndex;
    if (node.drawCount) {
      state.accumulate.drawIndex = initialDrawIndex * node.drawCount + nodeDrawIndex;
    }
    state.tileIndex = tileIndex;
    state.frame += state.accumulate.drawIndex;

    drawNode(node, state);

    // TODO: we can crash webgl when rendering large images with many samples
    // add an option to insert a setTimeout or requestAnimationFrame wait here
    // to break up the draws

    state.accumulate.drawIndex = initialDrawIndex;

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

    drawNodes(state, nodeIndex, nodeDrawIndex, tileIndex);
  }

  let projectDraw;

  if (project.createDraw) {
    projectDraw = project.createDraw(uniforms);
  }

  const clearScreenCmd = ctx.pass({
    clearColor: [0, 0, 0, 1],
    clearDepth: 1,
  });

  let frame = 0;

  let sync;

  let ready = () => {
    if ( ! sync) {
      return true;
    }
    let signaled = ctx.gl.getSyncParameter(sync, ctx.gl.SYNC_STATUS);
    let complete = signaled == ctx.gl.SIGNALED;
    if (complete) {
      ctx.gl.deleteSync(sync);
      sync = null;
    }
    return complete;
  }

  let draw = (state) => {

    pexHelpers.poll();

    state.frame = frame++;

    ctx.apply(clearScreenCmd);

    if (projectDraw) {
      projectDraw(state, () => {
        drawNodes(state, 0, 0, 0);
      })
    } else {
      drawNodes(state, 0, 0, 0);
    }

    if (webgl2) {
      sync = ctx.gl.fenceSync(ctx.gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    }

    firstPass = false;
  }

  let resize = (width, height) => {
    ctx.set({ width: width, height: height });
  }

  return {
    ready, draw, resize
  }
};
