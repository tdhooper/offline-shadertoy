const fs = require('fs');
const { mat4 } = require('gl-matrix');
const parseOBJ = require('parse-wavefront-obj');
const glslify = require('glslify');


const meshData = fs.readFileSync('projects/peel/model4.obj');
var mesh = parseOBJ(meshData);

const model = mat4.create();
mat4.rotateX(model, model, -.38);
mat4.rotateY(model, model, .56);
mat4.rotateZ(model, model, .01);
mat4.translate(model, model, [.222,-.5,.15]);
mat4.scale(model, model, [50, 50, 50]);

const init = function(uniforms) {
  const uu = Object.assign({}, uniforms);
  uu.model = model;

  const drawPolygons = global.regl({
    // primitive: 'lines',
    vert: `
      precision mediump float;
      attribute vec3 position, normal;
      uniform mat4 model, view, projection;
      varying vec3 vnormal;
      void main() {
        vnormal = normalize((model * vec4(normal, 1)).xyz);
        gl_Position = projection * view * model * vec4(position, 1);
      }
    `,
    frag: glslify`
      #extension GL_OES_standard_derivatives : enable
      precision mediump float;
      uniform bool guiSplit;
      varying vec3 vnormal;
      void main() {
        vec3 color = vnormal * .5 + .5;
        if ( ! guiSplit) {
          color = vec3(1) * pow(clamp(dot(vec3(0,.5,1.5), vnormal) * .5 + .5, 0., 1.), 1./2.2);
        }
        gl_FragColor = vec4(color, 1);
      }
    `,
    attributes: {
      position: mesh.positions,
      normal: mesh.vertexNormals,
    },
    elements: mesh.cells,
    uniforms: uu,
  });

  return drawPolygons;
}

module.exports = init;
// module.exports = function(){};
