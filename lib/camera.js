const { mat4, vec3, quat } = require('gl-matrix');
const pressed = require('key-pressed');
const FreeFlyCamera = require('./free-fly-camera');
const createMouse = require('./mouse');


function Camera(canvas, options) {
  if (!(this instanceof Camera)) return new Camera(canvas, options);
  FreeFlyCamera.call(this, options);

  this.lastMouse = undefined;
  this.lastTime = undefined;
  this.mouse = createMouse(canvas);
}

Camera.prototype = Object.create(FreeFlyCamera.prototype);
Camera.prototype.constructor = Camera;

Camera.prototype.tick = function() {
  this.lastMouse = this.lastMouse || this.mouse;

  const down = this.mouse[2] && ! this.lastMouse[2];

  if (down) {
    this.lastMouse = this.mouse;
  }

  const realTime = performance.now();
  const elapsed = this.lastTime ? realTime - this.lastTime : 0;
  this.lastTime = realTime;
  this.control(
    elapsed,
    [
      pressed('W'), pressed('S'),
      pressed('A'), pressed('D'),
      pressed('R'), pressed('F'),
      pressed('Q'), pressed('E'),
    ],
    this.mouse,
    this.lastMouse,
    pressed('<shift>')
  );
  this.lastMouse = this.mouse.slice();
};

Camera.prototype.toState = function() {
  return {
    rotation: Array.from(this.rotation),
    position: Array.from(this.position),
  };
};

Camera.prototype.fromState = function(state) {
  if (state.rotation && state.position) {
    if (state.rotation.length == 3) {
      // Convert from first-person-camera
      const out = mat4.create();
      mat4.translate(out, out, state.position);
      mat4.rotateX(out, out, state.rotation[0]);
      mat4.rotateY(out, out, state.rotation[1]);
      mat4.rotateZ(out, out, state.rotation[2] - Math.PI);
      mat4.getRotation(this.rotation, out);
      mat4.getTranslation(this.position, out);
    } else {
      quat.copy(this.rotation, state.rotation);
      vec3.copy(this.position, state.position);
    }
  } else if ('0' in state) {
    // Convert from old free-fly-camera
    mat4.getRotation(this.rotation, state);
    mat4.getTranslation(this.position, state);
  }
};


module.exports = Camera;
