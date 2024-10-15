const { mat4, vec3 } = require('gl-matrix');
const glslify = require('glslify');
const THREE = require('three');
const { TransformControls } = require('./TransformControls');

const createDraw = function(uniforms, setupProjectionView, draw, camera, project) {

  const evalGizmoResults = regl.framebuffer({
    width: 10,
    height: 2,
    colorType: 'float',
  });

  var evalGizmoPositions = regl.texture({
    width: 10,
    height: 2,
    format: 'rgb',
    type: 'float'
  });

  const configureEvalGizmos = (positions) => {

    // Update input buffer data
    let dataA = positions[0].reduce((acc, p) => {return acc.concat([p[0], p[1], p[2]])}, []);
    let dataB = positions[1].reduce((acc, p) => {return acc.concat([p[0], p[1], p[2]])}, []);
    let data = [].concat(dataA, dataB);
    evalGizmoPositions({
      width: positions[0].length,
      height: positions.length,
      format: 'rgb',
      type: 'float',
      data: data,
    });

    // Set results framebuffer size
    if (evalGizmoResults.width != positions.length) {
      evalGizmoResults.resize(evalGizmoPositions.width, evalGizmoPositions.height);
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
      position: [
        [-2, 0],
        [0, -2],
        [2, 2],
      ],
    },
    count: 3,
    uniforms: {
      evalGizmoPositions: evalGizmoPositions,
      evalGizmoPositionsResolution: (context, props) => {
        return [evalGizmoPositions.width, evalGizmoPositions.height];
      },
      gizmoAdjustmentMatrix: m4identity,
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

  const findOrigins = () => {

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

    configureEvalGizmos([positions, positions]);
    drawEvalGizmo();

    var bytes = new Float32Array(6 * 4 * 2);

    regl.read({
      framebuffer: evalGizmoResults,
      data: bytes,
    });

    let origins = [];

    for (let i = 0; i < 2; i++) {
      
      let offset = 6 * 16 * i;

      let x = inverseLerpOrigin(
        new Float32Array(bytes.buffer, 0 * 16 + offset, 3),
        new Float32Array(bytes.buffer, 1 * 16 + offset, 3),
        searchRadius
      );
  
      let y = inverseLerpOrigin(
        new Float32Array(bytes.buffer, 2 * 16 + offset, 3),
        new Float32Array(bytes.buffer, 3 * 16 + offset, 3),
        searchRadius
      );
  
      let z = inverseLerpOrigin(
        new Float32Array(bytes.buffer, 4 * 16 + offset, 3),
        new Float32Array(bytes.buffer, 5 * 16 + offset, 3),
        searchRadius
      );

      origins.push([x, y, z]);  
    }
    
    return origins;
  }

  const findJacobians = (origins) => {

    let searchRadius = 1/2;

    let positionsA = [
      vec3.create(),
      vec3.create(),
      vec3.create(),
    ];

    let positionsB = [
      vec3.create(),
      vec3.create(),
      vec3.create(),
    ];

    vec3.add(positionsA[0], origins[0], [searchRadius, 0, 0]),
    vec3.add(positionsA[1], origins[0], [0, searchRadius, 0]),
    vec3.add(positionsA[2], origins[0], [0, 0, searchRadius]),
    
    vec3.add(positionsB[0], origins[1], [searchRadius, 0, 0]),
    vec3.add(positionsB[1], origins[1], [0, searchRadius, 0]),
    vec3.add(positionsB[2], origins[1], [0, 0, searchRadius]),

    configureEvalGizmos([positionsA, positionsB]);
    drawEvalGizmo();

    var bytes = new Float32Array(positionsA.length * 4 * 2);

    regl.read({
      framebuffer: evalGizmoResults,
      data: bytes,
    });

    let jacobians = [];

    for (let i = 0; i < 2; i++) {
      let offset = 3 * 16 * i;

      let x = new Float32Array(bytes.buffer, 0 * 16 + offset, 3);
      let y = new Float32Array(bytes.buffer, 1 * 16 + offset, 3);
      let z = new Float32Array(bytes.buffer, 2 * 16 + offset, 3);
  
      x = vec3.scale(x, x, 1 / searchRadius);
      y = vec3.scale(y, y, 1 / searchRadius);
      z = vec3.scale(z, z, 1 / searchRadius);
    
      jacobians.push(mat4.fromValues(
        x[0], x[1], x[2], 0,
        y[0], y[1], y[2], 0,
        z[0], z[1], z[2], 0,
        0, 0, 0, 1,
      ));
    }

    return jacobians;
  }

  const findGizmoTransforms = () => {
    let origins = findOrigins();
    let originA = origins[1];
    let originB = origins[0];
    
    let jacobians = findJacobians(origins);
    let jacobianA = jacobians[0];
    let jacobianB = jacobians[1];
    
    mat4.invert(jacobianA, jacobianA);
    mat4.invert(jacobianB, jacobianB);
    
    const trsA = mat4.create();
    mat4.fromTranslation(trsA, originA);
    mat4.multiply(trsA, trsA, jacobianA);

    const trsB = mat4.create();
    mat4.fromTranslation(trsB, originB);
    mat4.multiply(trsB, trsB, jacobianB);

    return {
      initial: trsA,
      adjusted: trsB,
    };
  }

  const save = () => {
    mat4.invert(combinedAdjustmentMatrix, transforms.adjusted);
    mat4.multiply(combinedAdjustmentMatrix, combinedAdjustmentMatrix, transforms.initial);
    mat4.multiply(combinedAdjustmentMatrix, combinedAdjustmentMatrix, gizmoAdjustmentMatrix);
    
    fetch('/save-gizmo', {
      method : "POST",
      headers: {
        'Content-Type': 'application/json',
      },
      body : JSON.stringify({
        'files': Object.values(project.shaders).map(shader => shader.file),
        'matrix': Array.from(combinedAdjustmentMatrix),
      })
    }).then(
        response => response.text() // .json(), etc.
        // same as function(response) {return response.text();}
    ).then(
        html => console.log(html)
    );
  }

  const cameraMatrix = new THREE.Matrix4();
  const controlObjectMatrix = new THREE.Matrix4();

  const gizmoAdjustmentMatrix = mat4.create();
  const combinedAdjustmentMatrix = mat4.create();
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
  control.addEventListener( 'change', () => {
    draw(true, () => {})
  } );
  control.addEventListener( 'dragging-changed', function ( event ) {
    camera.moveEnabled = ! event.value;
    if (!event.value) {
      save();
    }
  });
  control.attach( controlObject );
  control.setMode( 'translate' );
  
  scene.add( control );
  
  let transforms;
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
      transforms = findGizmoTransforms();
      mat4.invert(inverseModel, transforms.adjusted);

      controlObjectMatrix.fromArray(transforms.adjusted);
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
