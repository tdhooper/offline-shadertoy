const createCube = require('primitive-cube');
const { mat4 } = require('gl-matrix');
const glslify = require('glslify');

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

const model = mat4.create();
mat4.rotateX(model, model, -.38);
mat4.rotateY(model, model, .56);
mat4.rotateZ(model, model, .01);
mat4.translate(model, model, [.222,-.5,.15]);
//mat4.scale(model, model, [50, 50, 50]);

const createDraw = function(uniforms, setupProjectionView) {

  const polyUniforms = Object.assign({}, uniforms);
  polyUniforms.model = model;

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
    uniforms: polyUniforms,
  });

  const evalPositions = [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1],
    [10, 11, 12],
    [13, 14, 15],
    [16, 17, 18],
  ];

  const evalGizmoResults = regl.framebuffer({
    width: evalPositions.length,
    height: 1,
    colorType: 'float',
  }); 
  
  const drawEvalGizmo = regl({
    vert: glslify('./eval-gizmo.vert'),
    frag: glslify('./eval-gizmo.frag'),
    attributes: {
      position: evalPositions,
      index: Array(evalPositions.length).fill().map((x, i) => i),
    },
    count: evalPositions.length,
    primitive: 'points',
    uniforms: {
      count: evalPositions.length,
    },
    framebuffer: evalGizmoResults,
  });

  uniforms['evalGizmoResults'] = evalGizmoResults;

  return function draw(state, drawShader) {
    setupProjectionView(state, (context) => {
      drawPolygons(state);
    });
    drawEvalGizmo();

    var pixels = regl.read({
      framebuffer: evalGizmoResults
    })

    console.log(pixels);

    drawShader();
  };
};

module.exports = createDraw;
