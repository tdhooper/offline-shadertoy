
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

const cmdProp = (name) => {
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

const evalUniforms = (props, context, commandArgs) => {
  const evaluatedProps = {};
  Object.entries(props).forEach(([name, prop]) => {
    const value = evalProp(prop, context, commandArgs);
    if (name.slice(-4) == '[]{}') {
      for (let i = 0; i < value.length; i++) {
        Object.entries(value[i]).forEach(([valueKey, valueValue]) => {
          evaluatedProps[name.slice(0, -3) + i + '].' + valueKey] = valueValue;
        });
      }
    } else if (name.slice(-2) == '[]') {
      for (let i = 0; i < value.length; i++) {
        evaluatedProps[name.slice(0, -1) + i + ']'] = value[i];
      }
    } else if (name.slice(-2) == '{}') {
      Object.entries(value).forEach(([valueKey, valueValue]) => {
        evaluatedProps[name.slice(0, -2) + '.' + valueKey] = valueValue;
      });
    } else {
      evaluatedProps[name] = value;
    }
  });
  return evaluatedProps;
}

const evalCmd = (cmd, commandArgs) => {

  cmd = Object.assign({}, cmd);

  let context = {
    drawingBufferWidth: ctx.gl.drawingBufferWidth,
    drawingBufferHeight: ctx.gl.drawingBufferHeight,
    framebufferWidth: ctx.gl.drawingBufferWidth,
    framebufferHeight: ctx.gl.drawingBufferHeight,
    viewportWidth: ctx.gl.drawingBufferWidth,
    viewportHeight: ctx.gl.drawingBufferHeight,
  };

  cmd.pass = evalProp(cmd.pass, context, commandArgs);

  if (cmd.pass && cmd.pass.framebuffer) {
    context.framebufferWidth = cmd.pass.framebuffer.width;
    context.framebufferHeight = cmd.pass.framebuffer.height;
    context.viewportWidth = cmd.pass.framebuffer.width;
    context.viewportHeight = cmd.pass.framebuffer.height;
  } else if (ctx.state.framebuffer) {
    context.framebufferWidth = ctx.state.framebuffer.width;
    context.framebufferHeight = ctx.state.framebuffer.height;
    context.viewportWidth = ctx.state.framebuffer.width;
    context.viewportHeight = ctx.state.framebuffer.height;
  }

  cmd.viewport = evalProp(cmd.viewport, context, commandArgs);

  if (cmd.viewport) {
    context.viewportWidth = cmd.viewport[2];
    context.viewportHeight = cmd.viewport[3];
  } 

  Object.entries(cmd).forEach(([name, prop]) => {
    if (name == 'uniforms') {
      cmd[name] = evalUniforms(prop, context, commandArgs);
    } else {
      cmd[name] = evalProp(prop, context, commandArgs);
    }
  });

  return cmd;
};

const evalCmds = (cmds, commandArgs) => {
  let cmd = {};

  cmds.forEach((thisCmd) => {
    let uniforms = {};
    if (cmd.uniforms || thisCmd.uniforms) {
      Object.assign(uniforms, cmd.uniforms, thisCmd.uniforms);
    }
    Object.assign(cmd, thisCmd);
    cmd.uniforms = uniforms;
  })

  return evalCmd(cmd, commandArgs);
}

let poll = () => {
  // taken from ctx.frame
  let gl = ctx.gl;
  if (ctx.updatePixelRatio) {
    ctx.pixelRatio = ctx.updatePixelRatio;
    // we need to reaply width/height and update styles
    if (!ctx.updateWidth) {
      ctx.updateWidth =
        parseInt(gl.canvas.style.width) || gl.canvas.width;
    }
    if (!ctx.updateHeight) {
      ctx.updateHeight =
        parseInt(gl.canvas.style.height) || gl.canvas.height;
    }
    ctx.updatePixelRatio = 0;
  }
  if (ctx.updateWidth) {
    gl.canvas.style.width = `${ctx.updateWidth}px`;
    gl.canvas.width = ctx.updateWidth * ctx.pixelRatio;
    ctx.updateWidth = 0;
  }
  if (ctx.updateHeight) {
    gl.canvas.style.height = `${ctx.updateHeight}px`;
    gl.canvas.height = ctx.updateHeight * ctx.pixelRatio;
    ctx.updateHeight = 0;
  }
  if (
    ctx.defaultState.viewport[2] !== gl.drawingBufferWidth ||
    ctx.defaultState.viewport[3] !== gl.drawingBufferHeight
  ) {
    ctx.defaultState.viewport[2] = gl.drawingBufferWidth;
    ctx.defaultState.viewport[3] = gl.drawingBufferHeight;
    ctx.defaultState.pass.framebuffer.width = gl.drawingBufferWidth;
    ctx.defaultState.pass.framebuffer.height = gl.drawingBufferHeight;
    gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
  }
  if (ctx.queries.length) {
    ctx.queries = this.queries.filter((q) => !q._available(this, q));
  }
}

const createPass = (options) => {

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

  const passOptions = {
    clearColor: [0, 0, 0, 1],
    clearDepth: 1,
    color: [tex],
  }

  if (options.depthTexture) {
    const depthTexOptions = Object.assign({}, texOptions);
    depthTexOptions.pixelFormat = ctx.PixelFormat.DEPTH_COMPONENT24;
    const depthTex = ctx.texture2D(depthTexOptions);
    passOptions.depth = depthTex;
  }

  let passCmd = ctx.pass(passOptions);

  return passCmd;
}

const passTex = (passCmd) => {
  return passCmd.framebuffer.color[0].texture
}

const passDepth = (passCmd) => {
  return passCmd.framebuffer.depth.texture
}

const resizePass = (passCmd, width, height) => {
  ctx.update(passCmd.framebuffer.color[0].texture, {width, height});
  let framebufferUpdate = {color: passCmd.framebuffer.color};
  if (passCmd.framebuffer.depth) {
    ctx.update(passCmd.framebuffer.depth.texture, {width, height});
    framebufferUpdate.depth = passCmd.framebuffer.depth;
  }
  ctx.update(passCmd.framebuffer, framebufferUpdate);
}

const read = (options) => {
  ctx.gl.bindFramebuffer(ctx.gl.FRAMEBUFFER, options.framebuffer.handle);
  const width = options.framebuffer.color[0].texture.width;
  const height = options.framebuffer.color[0].texture.height;
  ctx.gl.pixelStorei(ctx.gl.PACK_ALIGNMENT, 4)
  ctx.gl.readPixels(0, 0, width, height, ctx.gl.RGBA, ctx.gl.FLOAT, options.data);
}

export {
  cmdProp,
  evalCmd,
  evalCmds,
  read,
  poll,
  createPass,
  passTex,
  passDepth,
  resizePass,
};


