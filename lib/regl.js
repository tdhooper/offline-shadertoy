import isEmpty from "is-empty";
import createContext from "pex-context";

const createRegl = (options) => {

  let contextOptions = {
    type: 'webgl',
  };

  if ('pixelRatio' in options) {
    contextOptions.pixelRatio = options.pixelRatio;
  }

  const ctx = createContext(contextOptions);

  ctx.gl.getExtension("EXT_frag_depth");

  //ctx.debug(true);

  if (options.container) {
    options.container.appendChild(ctx.gl.canvas);
  };

  const globalContext = {
    pixelRatio: ctx.pixelRatio,
  };

  const setDefaultContext = () => {
    globalContext.drawingBufferWidth = ctx.gl.drawingBufferWidth;
    globalContext.drawingBufferHeight = ctx.gl.drawingBufferHeight;
    globalContext.framebufferWidth = ctx.gl.drawingBufferWidth;
    globalContext.framebufferHeight = ctx.gl.drawingBufferHeight;
    globalContext.viewportWidth = ctx.gl.drawingBufferWidth;
    globalContext.viewportHeight = ctx.gl.drawingBufferHeight;
  }

  setDefaultContext();

  const getProperty = (propertyName, object) => {
    var parts = propertyName.split( "." ),
      length = parts.length,
      i,
      property = object || this;
  
    for ( i = 0; i < length; i++ ) {
      property = property[parts[i]];
    }
  
    return property;
  }

  const prop = (name) => {
    return (context, commandArgs) => {
      return getProperty(name, commandArgs);
    }
  }

  const evalProp = (prop, context, commandArgs) => {
    if (typeof prop === 'function') {
      return prop(context, commandArgs);
    }
    return prop;
  }

  const evalProps = (props, context, commandArgs) => {
    const evaluatedProps = {};
    Object.entries(props).forEach(([name, prop]) => {
      evaluatedProps[name] = evalProp(prop, context, commandArgs);
    });
    return evaluatedProps;
  }

  const evalPropRecursive = (prop, context, commandArgs) => {
    let evaluatedProp;
    if (typeof prop === 'function') {
      evaluatedProp = prop(context, commandArgs);
    } else if (typeof prop == 'object' && ! Array.isArray(prop)) {
      evaluatedProp = {};
      Object.entries(prop).forEach(([name, subProp]) => {
        evaluatedProp[name] = evalPropRecursive(subProp, context, commandArgs);
      });
    } else {
      evaluatedProp = prop;
    }
    return evaluatedProp;
  }

  let setContextFromFramebuffer = (framebuffer) => {
    if (framebuffer) {
      globalContext.framebufferWidth = framebuffer.passCmd.framebuffer.width;
      globalContext.framebufferHeight = framebuffer.passCmd.framebuffer.width;
    } else {
      globalContext.framebufferWidth = ctx.gl.drawingBufferWidth;
      globalContext.framebufferHeight = ctx.gl.drawingBufferHeight;
    }
  }

  let setContextFromViewport = (viewport) => {
    globalContext.viewportWidth = viewport.width;
    globalContext.viewportHeight = viewport.height;
  }

  let poll = () => {
    setDefaultContext();
  }

  const clear = (options) => {
    let passCmd;

    if ('framebuffer' in options) {
      passCmd = Object.assign({}, options.framebuffer.passCmd);
    } else {
      passCmd = ctx.pass({});
    }

    if ('color' in options) {
      passCmd.clearColor = options.color;
    }

    if ('depth' in options) {
      passCmd.clearDepth = options.depth;
    }

    ctx.submit({
      pass: passCmd
    });
  }

  const framebuffer = (options) => {

    const passOptions = {};

    const texOptions = {};

    if ('width' in options) {
      texOptions.width = options.width;
    }

    if ('height' in options) {
      texOptions.height = options.height;
    }

    if ('pixelFormat' in options) {
      texOptions.pixelFormat = options.pixelFormat;
    }

    const tex = ctx.texture2D(texOptions);
    passOptions.color = [tex];

    if (options.depthTexture) {
      const depthTexOptions = Object.assign({}, texOptions);
      depthTexOptions.pixelFormat = ctx.PixelFormat.DEPTH_COMPONENT32F;
      const depthTex = ctx.texture2D(depthTexOptions);
      passOptions.depth = depthTex;
    }

    let passCmd = ctx.pass(passOptions);

    const resize = (width, height) => {
      ctx.update(tex, {
        width,
        height,
      });
      if (passCmd.depth) {
        ctx.update(passCmd.depth, {
          width,
          height,
        });
      }
    }

    return {
      passCmd: passCmd,
      resize,
      width: passCmd.framebuffer.width,
      height: passCmd.framebuffer.height,
    };
  }

  const transformTextureOptions = (options) => {
    const texOptions = {};

    const wrapMap = {
      repeat: ctx.Wrap.ClampToEdge,
      clamp: ctx.Wrap.Repeat,
      mirror: ctx.Wrap.MirroredRepeat
    }

    const filterMap = {
      nearest: ctx.Filter.Nearest,
      linear: ctx.Filter.Linear,
      mipmap: ctx.Filter.LinearMipmapLinear,
      'linear mipmap linear': ctx.Filter.LinearMipmapLinear,
      'nearest mipmap nearest': ctx.Filter.NearestMipmapNearest,
      'nearest mipmap linear': ctx.Filter.NearestMipmapLinear,
      'linear mipmap nearest': ctx.Filter.LinearMipmapNearest,
    }

    if ('wrapS' in options) {
      texOptions.wrapS = options.wrapS in wrapMap ? wrapMap[options.wrapS] : null;
    }

    if ('wrapT' in options) {
      texOptions.wrapT = options.wrapT in wrapMap ? wrapMap[options.wrapT] : null;
    }

    if ('min' in options) {
      texOptions.min = options.min in filterMap ? filterMap[options.min] : null;
    }

    if ('mag' in options) {
      texOptions.mag = options.mag in filterMap ? filterMap[options.mag] : null;
    }

    if ('mipmap' in options) {
      texOptions.mipmap = options.mipmap;
    }

    if ('data' in options) {
      texOptions.data = options.data;
    }

    if ('flipY' in options) {
      texOptions.flipY = options.flipY;
    }

    if ('width' in options) {
      texOptions.width = options.width;
    }

    if ('height' in options) {
      texOptions.height = options.height;
    }

    return texOptions;
  }

  const texture = (options) => {
    const texOptions = transformTextureOptions(options);
    const tex = ctx.texture2D(texOptions);
    const update = (options) => {
      let texOptions = transformTextureOptions(options);
      ctx.update(tex, texOptions);
    }
    Object.assign(update, tex);
    return update;
  }

  const read = (options) => {
    const width = options.framebuffer.passCmd.framebuffer.width;
    const height = options.framebuffer.passCmd.framebuffer.height;
    ctx.gl.readPixels(0, 0, width, height, ctx.gl.RGBA, ctx.gl.FLOAT, options.data);
  }

  let cmdStack = [];

  const createCommand = (options) => {

    let attributes = {};
    if ('attributes' in options) {
      Object.entries(options.attributes).forEach(([name, attribute]) => {
        attributes[name] = ctx.vertexBuffer(attribute);
      });
    }

    return (commandArgs, subCommand, doneCallback) => {

      const firstCmd = cmdStack.length == 0;

      if ('framebuffer' in options) {
        setContextFromFramebuffer(evalProp(options.framebuffer, globalContext, commandArgs));
      }

      if ('viewport' in options) {
        setContextFromViewport(evalProp(options.viewport, globalContext, commandArgs));
      }

      let cmd = {};

      if ('framebuffer' in options) {
        let framebuffer = evalProp(options.framebuffer, globalContext, commandArgs);
        if (framebuffer) {
          cmd.pass = framebuffer.passCmd;
        }
      }

      let pipelineCmd = {};

      if ('frag' in options) {
        pipelineCmd.frag = evalProp(options.frag, globalContext, commandArgs);
      }

      if ('vert' in options) {
        pipelineCmd.vert = evalProp(options.vert, globalContext, commandArgs);
      }

      if ( ! isEmpty(pipelineCmd)) {
        cmd.pipeline = ctx.pipeline(pipelineCmd);
      }

      if ('uniforms' in options) {
        cmd.uniforms = evalProps(options.uniforms, globalContext, commandArgs);
      }

      if ('scissor' in options) {
        let scissor = evalPropRecursive(options.scissor, globalContext, commandArgs);
        if (scissor.enable == true && scissor.box) {
          cmd.scissor = [scissor.box.x, scissor.box.y, scissor.box.width, scissor.box.height];
        }
      }

      if ('viewport' in options) {
        let viewport = evalProp(options.scissor, globalContext, commandArgs);
        if (viewport) {
          cmd.viewport = [viewport.x, viewport.y, viewport.width, viewport.height];
        }
      }

      if ('attributes' in options) {
        cmd.attributes = attributes;
      }

      if ('count' in options) {
        cmd.count = options.count;
      }

      cmdStack.push(cmd);

      if (subCommand != null) {
        // wites to cmdStack
        subCommand(globalContext);
      }

      if (firstCmd) {
        let mergedCommands = {};
        cmdStack.forEach((cmd) => {
          mergedCommands = ctx.mergeCommands(mergedCommands, cmd, true);
        })
        ctx.submit(mergedCommands, () => {
          if (doneCallback != null) {
            doneCallback();
          }
        });
        cmdStack = [];
      }
    }
  }

  createCommand._gl = ctx.gl;
  createCommand.prop = prop;
  createCommand.clear = clear;
  createCommand.framebuffer = framebuffer;
  createCommand.poll = poll;
  createCommand.frame = ctx.frame.bind(ctx);
  createCommand.ctx = ctx;
  createCommand.texture = texture;
  createCommand.read = read;

  return createCommand;
}

export default createRegl;


/*
1. copy regl shim
2. convert regl-like options to their pex format
3. merge drawraymarch and setupprojectionview with the main node comand
4. node defines its pipeline and pass (framebuffer) upfront
5. switch the buffer or screen pass call depending on node.final
6. the rest of the draw command can have props, and is evaluated just before drawing
7. turn new regl-shim into just an command options evaluator
*/


