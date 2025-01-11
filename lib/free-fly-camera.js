import { vec3, quat, mat4, mat3 } from 'gl-matrix';


const Camera = function(options) {
  this.position = options.position || vec3.create();
  this.rotation = options.rotation || quat.create();
  this.positionSpeed = options.positionSpeed || 5;
  this.tiltSpeed = options.tiltSpeed || 10;
  this.rotationSpeed = options.rotationSpeed || .5;
  this.shiftModifier = options.shiftModifier || 10;
  this._dir = vec3.create();
  this._dir2 = vec3.create();
  this._rot = quat.create();
  this._mat = mat4.create();

  this.simPosition = vec3.clone(this.position);
  this.prevPosition = vec3.clone(this.position);
  this.velocity = vec3.create();
  this.velocityDt = vec3.create();
  this.accelleration = vec3.create();
  this.accellerationDt = vec3.create();
  this.drag = vec3.create();

  this.currentTime = performance.now();
  this.accumulator = 0;
  this.simDt = 1;
};

Camera.prototype.view = function(out) {
  out = out || mat4.create();
  mat4.fromQuat(out, this.rotation);
  vec3.negate(this._dir, this.position);
  mat4.translate(out, out, this._dir);
  return out;
};

/*
  Move
  0: Forward
  1: Back
  2: Left
  3: Right
  4: Up
  5: Down
  6: Roll left
  7: Roll right
*/

Camera.prototype.control = function(dt, move, mouse, prevMouse, shift) {

  let positionSpeed = this.positionSpeed / 1000000;
  let tiltSpeed = this.tiltSpeed / 1000;

  if (shift) {
    positionSpeed *= this.shiftModifier;
    tiltSpeed *= this.shiftModifier;
  }

  let forward, back, left, right, up, down, rollRight, rollLeft;
  [ forward, back, left, right, up, down, rollRight, rollLeft ] = move;

  vec3.set(this._dir, right - left, up - down, back - forward);
  if (forward || back || left || right || up || down) {
    vec3.normalize(this._dir, this._dir);
    vec3.scale(this._dir, this._dir, positionSpeed);
  }

  let tilt = rollLeft - rollRight;
  tilt *= tiltSpeed * 100;

  this.move(this._dir);
  this.pointer(mouse, prevMouse, tilt);

  this.dt = dt;
};

Camera.prototype.integrate = function(dt) {

  vec3.scale(this.velocityDt, this.velocity, dt);
  vec3.add(this.simPosition, this.simPosition, this.velocityDt);

  vec3.scale(this.accellerationDt, this.accelleration, dt * dt * .5);
  vec3.add(this.simPosition, this.simPosition, this.accellerationDt);

  vec3.scale(this.accellerationDt, this.accelleration, dt);
  vec3.add(this.velocity, this.velocity, this.accellerationDt);
}

Camera.prototype.simulate = function() {
  const newTime = performance.now();
  let frameTime = newTime - this.currentTime;
  if (frameTime > 250) {
    frameTime = 250;
  }
  this.currentTime = newTime;
  this.accumulator += frameTime;

  while (this.accumulator >= this.simDt)
  {
      vec3.copy(this.prevPosition, this.simPosition);
      this.integrate(this.simDt);
      this.accumulator -= this.simDt;
  }

  const alpha = this.accumulator / this.simDt;

  vec3.lerp(this.position, this.prevPosition, this.simPosition, alpha);
}

Camera.prototype.resetSimulation = function() {
  vec3.copy(this.prevPosition, this.position);
  vec3.copy(this.simPosition, this.position);
  vec3.set(this.velocity, 0, 0, 0);
}

Camera.prototype.move = function(dir) {
  quat.invert(this._rot, this.rotation);
  vec3.transformQuat(this.accelleration, dir, this._rot);
  vec3.scale(this.drag, this.velocity, -0.0016 * Math.sqrt(this.positionSpeed));
  vec3.add(this.accelleration, this.accelleration, this.drag);
  
  let lastSpeed = vec3.sqrLen(this.velocity);
  
  this.simulate();
  
  let speed = vec3.sqrLen(this.velocity);
  if (speed < lastSpeed && speed < .00000001) {
    vec3.set(this.velocity, 0, 0, 0);
  }
};

Camera.prototype.pointer = function(da, db, tilt) {
  let x = da[0] - db[0];
  let y = da[1] - db[1];
  let z = tilt || 0;
  x *= this.rotationSpeed;
  y *= this.rotationSpeed;
  z *= this.rotationSpeed;
  quat.fromEuler(this._rot, y, x, z);
  quat.multiply(this.rotation, this._rot, this.rotation);
};

Camera.prototype.lookAtOrigin = function() {
  vec3.set(this._dir, 0, 0, 0);
  vec3.set(this._dir2, 0, 1, 0);
  mat4.lookAt(this._mat, this.position, this._dir, this._dir2);
  mat4.getRotation(this.rotation, this._mat);
};


export default Camera;
