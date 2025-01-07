import { mat4, vec3, quat } from 'gl-matrix';
import pressed from 'key-pressed';
import FreeFlyCamera from './free-fly-camera';


function Camera(mouse, options) {
  if (!(this instanceof Camera)) return new Camera(mouse, options);
  FreeFlyCamera.call(this, options);

  this.lastMouse = undefined;
  this.lastTime = undefined;
  this.mouse = mouse;
  this.moveEnabled = true;
}

Camera.prototype = Object.create(FreeFlyCamera.prototype);
Camera.prototype.constructor = Camera;

Camera.prototype.tick = function() {

  let mouseState = this.mouse.toState();

  this.lastMouse = this.lastMouse || mouseState;

  const down = mouseState[2] && ! this.lastMouse[2];

  if (down) {
    this.lastMouse = mouseState;
  }

  if ( ! this.moveEnabled) { return; }

  const realTime = performance.now();
  const elapsed = this.lastTime ? realTime - this.lastTime : 0;
  this.lastTime = realTime;
  this.control(
    elapsed/20,
    [
      pressed('W'), pressed('S'),
      pressed('A'), pressed('D'),
      pressed('E'), pressed('Q'),
      pressed('F'), pressed('R'),
    ],
    mouseState,
    this.lastMouse,
    pressed('<shift>')
  );
  this.lastMouse = mouseState;
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
  this.lastMouse = null;
};


export default Camera;
