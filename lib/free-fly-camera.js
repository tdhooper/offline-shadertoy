var glm = require('gl-matrix');
var vec3 = glm.vec3;
var mat4 = glm.mat4;
var quat = glm.quat;

function FreeFlyCamera(opts) {
  if (!(this instanceof FreeFlyCamera)) return new FreeFlyCamera(opts)
  opts = opts || {}
  this.position = opts.position || vec3.create()
  this.rotation = opts.rotation || vec3.create()
  this.positionSpeed = opts.positionSpeed || 10
  this.rotationSpeed = opts.rotationSpeed || .01
}
module.exports = FreeFlyCamera

FreeFlyCamera.prototype.view = function(out) {
  if (!out) out = mat4.create()
  mat4.rotateX(out, out, this.rotation[0])
  mat4.rotateY(out, out, this.rotation[1])
  mat4.rotateZ(out, out, this.rotation[2] - Math.PI)
  mat4.translate(out, out, [-this.position[0], -this.position[1], -this.position[2]])
  return out
}

FreeFlyCamera.prototype.control = function(dt, move, mouse, prevMouse) {
  var speed = (this.positionSpeed / 1000) * dt
  var dir = [0,0,0]
  if (move[0]) dir[2] -= speed
  else if (move[1]) dir[2] += speed
  if (move[2]) dir[0] += speed
  else if (move[3]) dir[0] -= speed
  if (move[4]) dir[1] -= speed
  else if (move[5]) dir[1] += speed
  this.move(dir)
  this.pointer(mouse, prevMouse)
}

FreeFlyCamera.prototype.move = function(dir) {
  if (dir[0] !== 0 || dir[1] !== 0 || dir[2] !== 0) {
    // var cam = mat4.create()
    vec3.rotateX(dir, dir, [0,0,0], this.rotation[0])
    vec3.rotateY(dir, dir, [0,0,0], this.rotation[1])
    // mat4.rotateY(cam, cam, this.rotation[1])
    // mat4.rotateZ(cam, cam, this.rotation[2] - Math.PI)
    // vec3.transformQuat(dir, dir, rot);
    // var cam = mat4.create()
    // mat4.rotateX(cam, cam, this.rotation[0])
    // var rot = mat4.getRotation([], cam);
    // vec3.transformMat4(dir, dir, cam)
    vec3.add(this.position, this.position, dir)
  }
}

FreeFlyCamera.prototype.pointer = function(da, db) {
  var dt = [da[0] - db[0], da[1]- db[1]]
  var rot = this.rotation
  rot[1] += dt[0] * this.rotationSpeed
  if (rot[1] < 0) rot[1] += Math.PI * 2
  if (rot[1] >= Math.PI * 2) rot[1] -= Math.PI * 2
  rot[0] += dt[1] * this.rotationSpeed
  if (rot[0] < -Math.PI * .5) rot[0] = -Math.PI*0.5
  if (rot[0] > Math.PI * .5) rot[0] = Math.PI*0.5
}
