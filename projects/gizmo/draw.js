const createCube = require('primitive-cube');
const { mat4, vec3 } = require('gl-matrix');
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

const evalGizmoResults = regl.framebuffer({
  width: 10,
  height: 1,
  colorType: 'float',
});

var evalGizmoPositions = regl.buffer({
  usage: 'dynamic',
  type: 'float',
  length: 1,
})

const configureEvalGizmos = (positions) => {
  // Update input buffer data
  evalGizmoPositions({
    data: positions.map((x, i) => [x[0], x[1], x[2], i])
  });
  
  // Set results framebuffer size
  if (evalGizmoResults.width != positions.length) {
    evalGizmoResults.resize(positions.length, 1);
  }

  // Clear results buffer
  regl.clear({
    color: [0,0,0,1],
    depth: 1,
    stencil: 0,
    framebuffer: evalGizmoResults,
  });
}

const drawEvalGizmo = regl({
  vert: glslify('./eval-gizmo.vert'),
  frag: glslify('./eval-gizmo.frag'),
  attributes: {
    position: evalGizmoPositions,
  },
  count: regl.prop('count'),
  primitive: 'points',
  uniforms: {
    count: regl.prop('count'),
  },
  framebuffer: evalGizmoResults,
});

/*
    pick 6 points in pairs of positive/negative cardinal directions around the starting point
    run them through Map - up until the code point we placed the gizmo
    given the line formed by each pair, find the closest point on the line to (0,0,0)
    do an inverse interpolation with this point on the line, to get a new starting coordinate value
    run the routine again with the new starting point
    stop when the closest point to (0,0,0) on each line is within some distance
    OPTIONAL: apply a scaling factor at each step so we don't overshoot
*/

const inverseLerpOrigin = (a, b) => {
  let direction = vec3.create();
  vec3.subtract(direction, b, a);
  return (-vec3.dot(direction, b) - vec3.dot(direction, a)) / 2;
}

const findOrigin = () => {

  let searchPoint = vec3.create(0, 0, 0);
  let searchRadius = .5;

  let positions = [
    vec3.create(),
    vec3.create(),
    vec3.create(),
    vec3.create(),
    vec3.create(),
    vec3.create(),
  ];

  vec3.add(positions[0], searchPoint, [-searchRadius, 0, 0]),
  vec3.add(positions[1], searchPoint, [searchRadius, 0, 0]),
  vec3.add(positions[2], searchPoint, [0, -searchRadius, 0]),
  vec3.add(positions[3], searchPoint, [0, searchRadius, 0]),
  vec3.add(positions[4], searchPoint, [0, 0, -searchRadius]),
  vec3.add(positions[5], searchPoint, [0, 0, searchRadius]),

  configureEvalGizmos(positions);
  
  drawEvalGizmo({
    count: positions.length
  });

  var bytes = new Float32Array(6 * 4);

  regl.read({
    framebuffer: evalGizmoResults,
    data: bytes,
  });

  let x = inverseLerpOrigin(
    new Float32Array(bytes.buffer, 0 * 16, 3),
    new Float32Array(bytes.buffer, 1 * 16, 3)
  ) / searchRadius / 2;

  let y = inverseLerpOrigin(
    new Float32Array(bytes.buffer, 2 * 16, 3),
    new Float32Array(bytes.buffer, 3 * 16, 3)
  ) / searchRadius / 2;

  let z = inverseLerpOrigin(
    new Float32Array(bytes.buffer, 4 * 16, 3),
    new Float32Array(bytes.buffer, 5 * 16, 3)
  ) / searchRadius / 2;

  configureEvalGizmos([[x, y, z]]);
  drawEvalGizmo({
    count: positions.length
  });
  let test = regl.read({
    framebuffer: evalGizmoResults,
  });
  //console.log(test);
  //console.log(x, y, z);

  return [x, y, z];
}

const findJacobian = (origin) => {

  let searchPoint = origin;
  let searchRadius = .001;

  let positions = [
    vec3.create(),
    vec3.create(),
    vec3.create(),
  ];

  vec3.add(positions[0], searchPoint, [searchRadius, 0, 0]),
  vec3.add(positions[1], searchPoint, [0, searchRadius, 0]),
  vec3.add(positions[2], searchPoint, [0, 0, searchRadius]),

  configureEvalGizmos(positions);
  
  drawEvalGizmo({
    count: positions.length
  });

  var bytes = new Float32Array(positions.length * 4);

  regl.read({
    framebuffer: evalGizmoResults,
    data: bytes,
  });

  let x = new Float32Array(bytes.buffer, 0 * 16, 3);
  let y = new Float32Array(bytes.buffer, 1 * 16, 3);
  let z = new Float32Array(bytes.buffer, 2 * 16, 3);

  x = vec3.scale(x, x, 1 / searchRadius);
  y = vec3.scale(y, y, 1 / searchRadius);
  z = vec3.scale(z, z, 1 / searchRadius);

  return mat4.fromValues(
    x[0], x[1], x[2], 0,
    y[0], y[1], y[2], 0,
    z[0], z[1], z[2], 0,
    0, 0, 0, 1,
  );
}

const createDraw = function(uniforms, setupProjectionView) {

  const polyUniforms = Object.assign({}, uniforms);
  polyUniforms.model = regl.prop('model');

  const model = mat4.create();

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

  return function draw(state, drawShader) {
    let origin = findOrigin();
    let jacobian = findJacobian(origin);
    
    mat4.invert(jacobian, jacobian);

    mat4.fromTranslation(model, origin);
    mat4.multiply(model, model, jacobian);

    setupProjectionView(state, (context) => {
      state.model = model;
      drawPolygons(state);
    });
    
    drawShader();
  };
};

module.exports = createDraw;
