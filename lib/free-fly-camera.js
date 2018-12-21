var glm = require('gl-matrix');
var vec3 = glm.vec3;
var mat4 = glm.mat4;
var quat = glm.quat;
const createMouse = require('./mouse');
const pressed = require('key-pressed');

function FreeFlyCamera(canvas, opts) {
    if (!(this instanceof FreeFlyCamera)) return new FreeFlyCamera(canvas, opts);
    opts = opts || {};
    this.position = opts.position || vec3.create();
    this.rotation = opts.rotation || quat.create();
    if (opts.view) {
        this.fromMatrix(opts.view);
    }
    this.positionSpeed = opts.positionSpeed || 1;
    this.rotationSpeed = opts.rotationSpeed || 5;

    this.lastMouse = undefined;
    this.lastTime = undefined;
    this.mouse = createMouse(canvas);
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

FreeFlyCamera.prototype.control = function(dt, move, mouse, prevMouse, faster) {
    var speed = (this.positionSpeed / 1000) * dt;
    if (faster) {
        speed *= 3;
    }
    var dir = [0,0,0];
    var tilt = 0;
    if (move[0]) dir[2] -= speed;
    else if (move[1]) dir[2] += speed;
    if (move[2]) dir[0] -= speed;
    else if (move[3]) dir[0] += speed;
    if (move[4]) dir[1] += speed;
    else if (move[5]) dir[1] -= speed;
    if (move[6]) tilt -= speed * 20;
    else if (move[7]) tilt += speed * 20;

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
    x *= this.rotationSpeed * .1;
    y *= this.rotationSpeed * .1;
    z = tilt * this.rotationSpeed * 1 || 0;
    var rotate = quat.create();
    quat.fromEuler(rotate, y, x, z);
    quat.multiply(this.rotation, rotate, this.rotation);
};

FreeFlyCamera.prototype.fromMatrix = function(matrix) {
    mat4.getRotation(this.rotation, matrix);
    mat4.getTranslation(this.position, matrix);
};

FreeFlyCamera.prototype.tick = function() {
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
          pressed('Q'), pressed('E')
        ],
        this.mouse.slice(0,2),
        this.lastMouse.slice(0,2),
        pressed('<shift>')
    );
    this.lastMouse = this.mouse.slice();
};
