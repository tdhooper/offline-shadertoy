const createCube = require('primitive-cube');
const { mat4 } = require('gl-matrix');

function offsetCells(cells, offset) {
  return cells.map(function(cell) {
    return cell.map(function(index) {
      return index + offset;
    });
  });
}

function mergeMeshes(meshes) {
  var positions = [];
  var normals = [];
  var uvs = [];
  var cells = [];
  var offset = 0;

  meshes.forEach(function(mesh) {
    positions = positions.concat(mesh.positions);
    normals = normals.concat(mesh.normals);
    uvs = normals.concat(mesh.uvs);
    cells = cells.concat(offsetCells(mesh.cells, offset));
    offset += mesh.positions.length;
  });

  return {
    positions: positions,
    normals: normals,
    uvs: uvs,
    cells: cells,
  };
}

const mesh = mergeMeshes([
  createCube(2, .1, .1),
  createCube(.1, 2, .1),
  createCube(.1, .1, 2),
]);

const createDraw = function(uniforms, setupProjectionView) {

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
    uniforms: uniforms,
  });

  return function draw(state, drawShader) {
    setupProjectionView(state, (context) => {
      drawPolygons(state);
    });
    drawShader();
  };
};

module.exports = createDraw;
