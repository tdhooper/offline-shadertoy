const createCube = require('primitive-cube');
const { mat4, vec3, quat } = require('gl-matrix');
const glslify = require('glslify');
const THREE = require('three');
const { TransformControls } = require('./TransformControls');

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

const inverseLerpOrigin = (a, b, searchRadius) => {
  let direction = vec3.create();
  vec3.subtract(direction, b, a);
  let t = (-vec3.dot(direction, b) - vec3.dot(direction, a));
  let len = vec3.length(direction);
  let scl = (searchRadius * 2) / len;
  t *= scl * scl;
  t /= searchRadius * 4;
  return t;
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
    new Float32Array(bytes.buffer, 1 * 16, 3),
    searchRadius
  );

  let y = inverseLerpOrigin(
    new Float32Array(bytes.buffer, 2 * 16, 3),
    new Float32Array(bytes.buffer, 3 * 16, 3),
    searchRadius
  );

  let z = inverseLerpOrigin(
    new Float32Array(bytes.buffer, 4 * 16, 3),
    new Float32Array(bytes.buffer, 5 * 16, 3),
    searchRadius
  );

  /*
  configureEvalGizmos([[x, y, z]]);
  drawEvalGizmo({
    count: positions.length
  });
  let test = regl.read({
    framebuffer: evalGizmoResults,
  });
  console.log(test);
  */
  //console.log(x, y, z);

  return [x, y, z];
}

const findJacobian = (origin) => {

  let searchPoint = origin;
  let searchRadius = 1/2;

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

  //console.log(x, y, z);

  return mat4.fromValues(
    x[0], x[1], x[2], 0,
    y[0], y[1], y[2], 0,
    z[0], z[1], z[2], 0,
    0, 0, 0, 1,
  );
}


const save = () => {
  fetch('/save-gizmo', {
    method : "POST",
    headers: {
      'Content-Type': 'application/json',
    },
    body : JSON.stringify({
      'foo': 2,
    })
  }).then(
      response => response.text() // .json(), etc.
      // same as function(response) {return response.text();}
  ).then(
      html => console.log(html)
  );
}

const saveButton = document.createElement('button');
saveButton.textContent = 'Save';
saveButton.classList.add('gizmo-save-button');
saveButton.addEventListener('click', save);
document.body.appendChild(saveButton);

const cameraMatrix = new THREE.Matrix4();
const controlObjectMatrix = new THREE.Matrix4();

const createDraw = function(uniforms, setupProjectionView, draw, camera) {

  const scene = new THREE.Scene();
  const threeCamera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.01, 1000 );

  const renderer = new THREE.WebGLRenderer({
    context: regl._gl,
    preserveDrawingBuffer: true,
  });
  renderer.autoClear = false;
  regl._refresh();
  
  const geometry = new THREE.BoxGeometry( 1.1, 1.1, 1.1 );
  const material = new THREE.MeshBasicMaterial( {
    color: 0x00ff00,
    depthTest: false,
    depthWrite: false,
    fog: false,
    toneMapped: false,
    transparent: true
  } );
 material.opacity = 0.15;


  //const controlObject = new THREE.Object3D();
  const controlObject = new THREE.Mesh(geometry, material);
  scene.add( controlObject );

  control = new TransformControls( threeCamera, regl._gl.canvas );
  let firstChange = true;
  control.addEventListener( 'change', () => {
    if (firstChange) {
      firstChange = false;
      return;
    }
    draw(true, () => {})
  } );
  control.addEventListener( 'dragging-changed', function ( event ) {
    camera.moveEnabled = ! event.value;
  } );
  control.attach( controlObject );
  control.setMode( 'translate' );
  
  scene.add( control );
  





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

  let resetGizmo = true;

  return function draw(state, drawShader) {

    cameraMatrix.fromArray(state.cameraMatrix);
    cameraMatrix.invert();
    cameraMatrix.decompose(threeCamera.position, threeCamera.quaternion, threeCamera.scale);
    threeCamera.aspect = window.innerWidth / window.innerHeight;
    threeCamera.setFocalLength((state.cameraFov * threeCamera.filmGauge / threeCamera.aspect));
    threeCamera.updateProjectionMatrix();

    if (resetGizmo) {
      let origin = findOrigin();
      let jacobian = findJacobian(origin);
      mat4.invert(jacobian, jacobian);
      mat4.fromTranslation(model, origin);
      mat4.multiply(model, model, jacobian);

      controlObjectMatrix.fromArray(model);
      controlObjectMatrix.decompose(controlObject.position, controlObject.quaternion, controlObject.scale);
      control.setSpace('local');

      resetGizmo = false;
    }

    //setupProjectionView(state, (context) => {
    //  state.model = model;
    //  drawPolygons(state);
    //});
    
    drawShader();
    
    renderer.resetState();
    renderer.render( scene, threeCamera );
    regl._refresh();

  };
};

module.exports = createDraw;
