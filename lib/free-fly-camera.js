const { vec3, quat, mat4, mat3 } = require('gl-matrix');


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
  let positionSpeed = (this.positionSpeed / 1000) * dt;
  let tiltSpeed = (this.tiltSpeed / 1000) * dt;
  positionSpeed/=2;
  tiltSpeed/=2;
  if (shift) {
    positionSpeed *= this.shiftModifier;
    tiltSpeed *= this.shiftModifier;
  }
  vec3.set(this._dir, 0, 0, 0);
  let tilt = 0;

  this._dir[2] -= move[0] ? positionSpeed : 0;
  this._dir[2] += move[1] ? positionSpeed : 0;

  this._dir[0] -= move[2] ? positionSpeed : 0;
  this._dir[0] += move[3] ? positionSpeed : 0;

  this._dir[1] += move[4] ? positionSpeed : 0;
  this._dir[1] -= move[5] ? positionSpeed : 0;

  tilt -= move[6] ? tiltSpeed * 100 : 0;
  tilt += move[7] ? tiltSpeed * 100 : 0;

  this.move(this._dir);
  this.pointer(mouse, prevMouse, tilt);
};

Camera.prototype.move = function(dir) {
  if (dir[0] === 0 && dir[1] === 0 && dir[2] === 0) {
    return;
  }
  quat.invert(this._rot, this.rotation);
  vec3.transformQuat(dir, dir, this._rot);
  vec3.add(this.position, this.position, dir);
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


module.exports = Camera;
