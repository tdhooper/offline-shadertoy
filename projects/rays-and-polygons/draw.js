const createCube = require('primitive-cube');
const { mat4 } = require('gl-matrix');


// 1 x 1 x 1 Box
const mesh = createCube();

const init = function(drawRaymarch, renderNodes, uniforms) {

  const drawPolygons = global.regl({
    vert: `
      precision mediump float;
      attribute vec3 position, normal;
      uniform mat4 model, view, projection;
      varying vec3 vnormal;
      void main() {
        vnormal = normal;
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
      normal: mesh.normals,
    },
    elements: mesh.cells,
    uniforms: {
      // model: function(context, props) {
      //   return mat4.fromTranslation([], props.camera.position);
      // }
      model: mat4.identity([]),
    },
  });

  return function draw(state, context) {
    drawPolygons(state);
    drawRaymarch(state, () => {
      renderNodes.forEach((node) => {
        node.draw(state);
      });
    });
  };
};

module.exports = init;
