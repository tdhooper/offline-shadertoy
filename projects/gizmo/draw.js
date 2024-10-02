const { mat4, vec3 } = require('gl-matrix');
const glslify = require('glslify');
const THREE = require('three');
const { TransformControls } = require('./TransformControls');

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

const m4identity = mat4.create();

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
    gizmoAdjustmentMatrix: m4identity
  },
  framebuffer: evalGizmoResults,
});

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

  const gizmoAdjustmentMatrix = mat4.create();
  const setupGizmoUniforms = regl({
    uniforms: {
      gizmoAdjustmentMatrix: regl.prop('gizmoAdjustmentMatrix'),
    },
  });

  const scene = new THREE.Scene();
  const threeCamera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.01, 1000 );

  const renderer = new THREE.WebGLRenderer({
    context: regl._gl,
    preserveDrawingBuffer: true,
  });
  renderer.autoClear = false;
  regl._refresh();
  
  const geometry = new THREE.BoxGeometry( 0, 0, 0 );
  const material = new THREE.MeshBasicMaterial( {
    color: 0x00ff00,
  } );

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
  

  const model = mat4.create();
  const inverseModel = mat4.create();

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
      mat4.invert(inverseModel, model);

      controlObjectMatrix.fromArray(model);
      controlObjectMatrix.decompose(controlObject.position, controlObject.quaternion, controlObject.scale);
      control.setSpace('local');

      resetGizmo = false;
    } else {
      mat4.multiply(gizmoAdjustmentMatrix, inverseModel, controlObject.matrixWorld.toArray())
      mat4.invert(gizmoAdjustmentMatrix, gizmoAdjustmentMatrix);
    }

    
    state.gizmoAdjustmentMatrix = gizmoAdjustmentMatrix;
    setupGizmoUniforms(state, () => {
      drawShader();
    });

    renderer.resetState();
    renderer.render( scene, threeCamera );
    regl._refresh();

  };
};

module.exports = createDraw;
