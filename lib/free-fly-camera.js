var glm = require('gl-matrix');
var vec3 = glm.vec3;
var mat4 = glm.mat4;
var quat = glm.quat;

function FreeFlyCamera(opts) {
    if (!(this instanceof FreeFlyCamera)) return new FreeFlyCamera(opts);
    opts = opts || {};
    this.position = opts.position || vec3.create();
    this.rotation = opts.rotation || quat.create();
    if (opts.view) {
        mat4.getRotation(this.rotation, opts.view);
        mat4.getTranslation(this.position, opts.view);
    }
    this.positionSpeed = opts.positionSpeed || 10;
    this.rotationSpeed = opts.rotationSpeed || 10;
}

module.exports = FreeFlyCamera;

FreeFlyCamera.prototype.view = function(out) {
    if (!out) out = mat4.create();
    mat4.fromRotationTranslation(out, this.rotation, this.position);
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

FreeFlyCamera.prototype.control = function(dt, move, mouse, prevMouse) {
    var speed = (this.positionSpeed / 1000) * dt;
    var dir = [0,0,0];
    var tilt = 0;
    if (move[0]) dir[2] -= speed;
    else if (move[1]) dir[2] += speed;
    if (move[2]) dir[0] -= speed;
    else if (move[3]) dir[0] += speed;
    if (move[4]) dir[1] += speed;
    else if (move[5]) dir[1] -= speed;
    if (move[6]) tilt -= speed;
    else if (move[7]) tilt += speed;

    this.move(dir);
    this.pointer(mouse, prevMouse, tilt);
};

FreeFlyCamera.prototype.move = function(dir) {
    if (dir[0] === 0 && dir[1] === 0 && dir[2] === 0) {
        return;
    }
    var inverted = quat.invert([], this.rotation);
    vec3.transformQuat(dir, dir, inverted);
    vec3.add(this.position, this.position, dir);
};

FreeFlyCamera.prototype.pointer = function(da, db, tilt) {
    var x = da[0] - db[0];
    var y = da[1] - db[1];
    x *= this.rotationSpeed * .06;
    y *= this.rotationSpeed * .06;
    z = tilt * this.rotationSpeed * 2 || 0;
    var rotate = quat.create();
    quat.fromEuler(rotate, y, x, z);
    quat.multiply(this.rotation, rotate, this.rotation);
};
