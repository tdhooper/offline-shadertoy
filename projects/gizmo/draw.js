const { mat4, vec3, quat } = require('gl-matrix');
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

  // given two coordinates in world-space
  // and their corresponding local space-coordinates
  // find the local-space origin, and return it in world space 
  const inverseLerpOrigin2 = (worldA, worldB, localA, localB) => {
    let direction = vec3.create();
    vec3.subtract(direction, b, a);
    let t = (-vec3.dot(direction, b) - vec3.dot(direction, a));
    let len = vec3.length(direction);
    let scl = (searchRadius * 2) / len;
    t *= scl * scl;
    t /= searchRadius * 4;
    return t;
  }

  const vec3FromArray = (array, index) => {
    return new Float32Array(array.buffer, index * 16, 3);
  }

  const findOriginTransformsFromSourcePoints = (points) => {

    let worldMatrices = points.map(point => {
      let world = mat4.create();
      mat4.fromRotationTranslationScale(
        world,
        quat.create(),
        point,
        vec3.fromValues(.01, .01, .01)
      );
      return world;
    });

    let positions = worldMatrices.reduce((acc, world) => {

      let positions = [
        vec3.fromValues(1, 0, 0),
        vec3.fromValues(0, 1, 0),
        vec3.fromValues(0, 0, 1),
        vec3.fromValues(0, 0, 0),
      ];

      positions.forEach(position => {
        vec3.transformMat4(position, position, world);
      });

      return acc.concat(positions);
    }, []);

    configureEvalGizmos([positions, positions]);
    drawEvalGizmo();

    let count = worldMatrices.length;
    var bytes = new Float32Array(count * 4 * 2 * 4);

    regl.read({
      framebuffer: evalGizmoResults,
      data: bytes,
    });

    let initialTransforms = [];
    let combinedTransforms = [];

    for (let i = 0; i < count * 2; i++) {

      let x = vec3FromArray(bytes, i * 4 + 0);
      let y = vec3FromArray(bytes, i * 4 + 1);
      let z = vec3FromArray(bytes, i * 4 + 2);
      let o = vec3FromArray(bytes, i * 4 + 3);

      vec3.sub(x, x, o);
      vec3.sub(y, y, o);
      vec3.sub(z, z, o);

      let local = mat4.fromValues(
        x[0], x[1], x[2], 0,
        y[0], y[1], y[2], 0,
        z[0], z[1], z[2], 0,
        o[0], o[1], o[2], 1,
      );

      let inverseWorld = mat4.create();
      mat4.invert(inverseWorld, worldMatrices[i % count]);
      mat4.multiply(local, local, inverseWorld);
      mat4.invert(local, local);

      if (i < count) {
        initialTransforms.push(local);
      } else {
        combinedTransforms.push(local);
      }
    }

    return {
      initial: initialTransforms,
      combined: combinedTransforms,
    };
  }

  const findGizmoTransforms = () => {
/*
    let points = [
      vec3.fromValues( 0,  0,  0),
    ];

    let size = 10;
    let count = 10;
    for (let x = 0; x < count; x++) {
      for (let y = 0; y < count; y++) {
        for (let z = 0; z < count; z++) {
          points.push(vec3.fromValues(
            (x / (count - 1)) * size - size / 2,
            (y / (count - 1)) * size - size / 2,
            (z / (count - 1)) * size - size / 2,
          ))
        }
      }
    }
    */

    let points = [
      vec3.fromValues( 0,  0,  0),
      vec3.fromValues( 1,  1,  1),
      vec3.fromValues( 1,  1, -1),
      vec3.fromValues( 1, -1,  1),
      vec3.fromValues( 1, -1, -1),
      vec3.fromValues(-1,  1,  1),
      vec3.fromValues(-1,  1, -1),
      vec3.fromValues(-1, -1,  1),
      vec3.fromValues(-1, -1, -1),
    ]

    let transforms = findOriginTransformsFromSourcePoints(points);

    let lowestScore = Infinity;
    let lowestScoreIndex = 0;
    let difference = mat4.create();

    transforms.initial.forEach((transform, i) => {

      let score = transforms.initial.reduce((totalScore, otherTransform) => {
        mat4.subtract(difference, transform, otherTransform);
        let differenceScore = difference.reduce((acc, value) => {return acc + Math.abs(value)}, 0);
        return totalScore + differenceScore;
      }, 0);

      if (score < lowestScore) {
        lowestScore = score;
        lowestScoreIndex = i;
      }
    });

    //console.log(lowestScore);

    return {
      initial: transforms.initial[lowestScoreIndex], 
      combined: transforms.combined[lowestScoreIndex],
    };
  }

  const save = () => {
    mat4.invert(combinedAdjustmentMatrix, transforms.combined);
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
    alpha: true
  });
  renderer.setSize(window.innerWidth, window.innerHeight);
  regl._gl.canvas.after(renderer.domElement);
  renderer.domElement.style.position = 'absolute';
  window.addEventListener('resize', () => {
    renderer.setSize(window.innerWidth, window.innerHeight);
  });
  
  const geometry = new THREE.BoxGeometry( 0, 0, 0 );
  const material = new THREE.MeshBasicMaterial( {
    color: 0x00ff00,
  } );

  const controlObject = new THREE.Mesh(geometry, material);
  scene.add( controlObject );

  control = new TransformControls( threeCamera, renderer.domElement );
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
  const gizmo = control.getHelper();
  scene.add( gizmo );
  control.setMode( 'translate' );
  
  let transforms;
  const inverseCombined = mat4.create();
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
      mat4.invert(inverseCombined, transforms.combined);

      controlObjectMatrix.fromArray(transforms.combined);
      controlObjectMatrix.decompose(controlObject.position, controlObject.quaternion, controlObject.scale);
      control.setSpace('local');

      resetGizmo = false;
    } else {
      mat4.multiply(gizmoAdjustmentMatrix, inverseCombined, controlObject.matrixWorld.toArray())
      mat4.invert(gizmoAdjustmentMatrix, gizmoAdjustmentMatrix);
    }

    state.gizmoAdjustmentMatrix = gizmoAdjustmentMatrix;
    setupGizmoUniforms(state, () => {
      drawShader();
    });

    renderer.render( scene, threeCamera );
  };
};

module.exports = createDraw;
