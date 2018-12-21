const vec3 = require('gl-matrix').vec3;
const FirstPersonCamera = require('first-person-camera');
const pressed = require('key-pressed');
const createMouse = require('./mouse');


function Camera(canvas, options) {
  if (!(this instanceof Camera)) return new Camera(canvas, options);
  FirstPersonCamera.call(this, options);

  this.lastMouse = undefined;
  this.lastTime = undefined;
  this.mouse = createMouse(canvas);
}

Camera.prototype = Object.create(FirstPersonCamera.prototype);
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
    ],
    this.mouse,
    this.lastMouse
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
    vec3.copy(this.rotation, state.rotation);
    vec3.copy(this.position, state.position);
  }
};


module.exports = Camera;
