const fs = require('fs');
const { mat4 } = require('gl-matrix');
const parseOBJ = require('parse-wavefront-obj');


const meshData = fs.readFileSync('projects/peel/model4.obj');
const mesh = parseOBJ(meshData);

const model = mat4.create();
mat4.rotateX(model, model, -.3);
mat4.rotateY(model, model, .56);
mat4.rotateZ(model, model, .01);
mat4.translate(model, model, [.225,-.5,.15]);
mat4.scale(model, model, [50, 50, 50]);

const drawPolygons = global.regl({
  vert: `
    precision mediump float;
    attribute vec3 position, normal;
    uniform mat4 model, view, projection;
    varying vec3 vnormal;
    void main() {
      vnormal = normalize(normal);
      gl_Position = projection * view * model * vec4(position, 1);
    }
  `,
  frag: `
    precision mediump float;
    varying vec3 vnormal;
    void main() {
      gl_FragColor = vec4(vnormal * .5 + .5, 1);
    }
  `,
  attributes: {
    position: mesh.positions,
    normal: mesh.vertexNormals,
  },
  elements: mesh.cells,
  uniforms: {
    // model: function(context, props) {
    //   return mat4.fromTranslation([], props.camera.position);
    // }
    model: model,
  },
});

module.exports = drawPolygons;
