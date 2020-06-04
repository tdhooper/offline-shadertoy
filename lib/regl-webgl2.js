const createREGL = require('regl/dist/regl.unchecked');

// https://github.com/regl-project/regl/issues/561
const overrideContextType = (forcedContextType, callback) => {
  // Monkey-patch context creation to override the context type
  const origGetContext = HTMLCanvasElement.prototype.getContext;
  HTMLCanvasElement.prototype.getContext = function(ignoredContextType, contextAttributes) {
    return origGetContext.bind(this)(forcedContextType, contextAttributes);
  };

  // Execute the callback with overridden context type
  const result = callback();

  // Restore the original method
  HTMLCanvasElement.prototype.getContext = origGetContext;

  return result;
};

const createREGLwebgl2 = options => overrideContextType('webgl2', () => createREGL(options));

module.exports = createREGLwebgl2;
