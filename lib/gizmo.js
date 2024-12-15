const { mat4, vec3, quat } = require('gl-matrix');
const THREE = require('three');
const { TransformControls } = require('./TransformControls');
const createGizmoGui = require('./gizmo-gui');

  // TODO: We need to process the shader before it gets turned into a
  // render node, else the regl evaluator throws at the missing
  // GIZMO function during compilation

let shaderFiles = [];

const preprocessShader = (shader) => {

  if (shader.glsl.indexOf('GIZMO(') == -1) {
    return;
  }

  shaderFiles.push(shader.file);

  shader.glsl = `
    precision mediump float;

    uniform mat4 gizmoAdjustmentMatrix;

    vec3 GIZMO_LOCAL_P;
    vec3 GIZMO_LOCAL_P2;
    bool GIZMO_SET = false;

    float GIZMO(inout vec3 p, mat4 m) {
        if ( ! GIZMO_SET) GIZMO_LOCAL_P = p;
        p = (m * vec4(p, 1)).xyz;
        float scale = length(m[0].xyz);
        if ( ! GIZMO_SET) GIZMO_LOCAL_P2 = p;
        p = (gizmoAdjustmentMatrix * vec4(p, 1)).xyz;
        scale *= length(gizmoAdjustmentMatrix[0].xyz);
        GIZMO_SET = true;
        return scale;
    }

    float GIZMO(inout vec3 p) {
        if ( ! GIZMO_SET) GIZMO_LOCAL_P = p;
        if ( ! GIZMO_SET) GIZMO_LOCAL_P2 = p;
        p = (gizmoAdjustmentMatrix * vec4(p, 1)).xyz;
        float scale = length(gizmoAdjustmentMatrix[0].xyz);
        GIZMO_SET = true;
        return scale;
    }
  ` + shader.glsl;
};

const createGizmo = (camera, mouse, renderNodes, uniforms) => {

  if (shaderFiles.length == 0) {
    console.log('no GIZMO functions in use');
    return {
      toState: () => {
        return {};
      },
      update: () => {}
    };
  }
  
  const gizmoAdjustmentMatrix = mat4.create();
  uniforms.gizmoAdjustmentMatrix = regl.prop('gizmoAdjustmentMatrix');

  // Find the multipass render node that includes the GIZMO_MAP function
  let mapNode;
  for (let i = renderNodes.length - 1; i >= 0; i--) {
    let node = renderNodes[i];
    if (node.shader.indexOf('GIZMO_MAP(') != -1) {
      mapNode = node;
    }
  }
  if ( ! mapNode) {
    console.log('Could not find GIZMO_MAP function');
    return {
      toState: () => {
        return {
          gizmoAdjustmentMatrix: gizmoAdjustmentMatrix,
        };
      },
      update: () => {}
    };
  }

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

  /*
    shader preprocess hook?
    * Add GIZMO(p) functions and uniform to all shaders with GIZMO call

    on create
    * Copy the render node, and set a new shader:
    * Rename the main() function to GIZMO_MAIN
    * Insert our own main() function, that runs GIZMO_MAIN (it might have setup code)
    * Insert GIZMO functions and uniforms
    * Call the GIZMO_MAP function with P values, and return as FragColor
  */

  let shader = mapNode.shader;
  let reMain = new RegExp("void[\\s\\n]*(main)[\\s\\n]*\\([\\s\\n]*\\)[\\s\\n]*{", "gd");
  let match = reMain.exec(shader);
  shader = shader.slice(0, match.indices[1][0]) + "GIZMO_MAIN" + shader.slice(match.indices[1][1]);

  let depth = shader.indexOf('gl_FragDepthEXT') != -1 ? 'gl_FragDepthEXT = 0.;' : '';

  shader += `
    uniform vec2 evalGizmoPositionsResolution;
    uniform sampler2D evalGizmoPositions;
    void main() {
      GIZMO_MAIN();
      vec3 position = texture2D(evalGizmoPositions, vec2(gl_FragCoord.x, gl_FragCoord.y) / evalGizmoPositionsResolution).rgb;
      GIZMO_SET = false;
      GIZMO_MAP(position);
      if (gl_FragCoord.y < 1.) {
        gl_FragColor = vec4(GIZMO_LOCAL_P, 0);
      } else {
        gl_FragColor = vec4(GIZMO_LOCAL_P2, 0);
      }
      
      ${depth}

      //gl_FragColor = vec4(gl_FragCoord.x, gl_FragCoord.y, 0, 0);
      //gl_FragColor = vec4(position, 0);
    }
  `;

  let gizmoUniforms = Object.assign({
    evalGizmoPositions: evalGizmoPositions,
    evalGizmoPositionsResolution: (context, props) => {
      return [evalGizmoPositions.width, evalGizmoPositions.height];
    },
  }, uniforms);

  const gizmoDataDraw = regl({
    frag: shader,
    uniforms: gizmoUniforms,
    framebuffer: evalGizmoResults,
  });
  
  const drawEvalGizmo = (state) => {
    mapNode.draw(state, () => {
      gizmoDataDraw(state);
    });
  }

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

  const vec3FromArray = (array, index) => {
    return new Float32Array(array.buffer, index * 16, 3);
  }

  const findOriginTransformsFromSourcePoints = (state, points) => {

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
    drawEvalGizmo(state);

    let count = worldMatrices.length;
    var bytes = new Float32Array(count * 4 * 2 * 4);

    regl.read({
      framebuffer: evalGizmoResults,
      data: bytes,
    });

    let transformsBefore = [];
    let transformsAfter = [];

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
      //mat4.invert(local, local);

      if (i < count) {
        transformsBefore.push(local);
      } else {
        transformsAfter.push(local);
      }
    }

    return {
      before: transformsBefore,
      after: transformsAfter,
    };
  }

  const findGizmoTransforms = (state) => {
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
    /*/
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
    //*/

    let transforms = findOriginTransformsFromSourcePoints(state, points);

    let lowestScore = Infinity;
    let lowestScoreIndex = 0;
    let difference = mat4.create();

    transforms.before.forEach((transform, i) => {

      let score = transforms.before.reduce((totalScore, otherTransform) => {
        mat4.subtract(difference, transform, otherTransform);
        let differenceScore = difference.reduce((acc, value) => {return acc + Math.abs(value)}, 0);
        return totalScore + differenceScore;
      }, 0);

      if (score < lowestScore) {
        lowestScore = score;
        lowestScoreIndex = i;
      }
    });

    return {
      // transformation before we apply the gizmo
      before: transforms.before[lowestScoreIndex],
      // transformation after we apply the gizmo
      after: transforms.after[lowestScoreIndex],
    };
  }

  const inverseAfter = mat4.create();
  const combinedAdjustmentMatrix = mat4.create();
  const cameraMatrix = new THREE.Matrix4();
  const controlObjectMatrix = new THREE.Matrix4();
  let transforms;
  let resetGizmo = true;

  const save = () => {
    mat4.multiply(combinedAdjustmentMatrix, transforms.before, controlObject.matrixWorld.toArray());
    mat4.invert(combinedAdjustmentMatrix, combinedAdjustmentMatrix);

    fetch('/save-gizmo', {
      method : "POST",
      headers: {
        'Content-Type': 'application/json',
      },
      body : JSON.stringify({
        'files': shaderFiles,
        'matrix': Array.from(combinedAdjustmentMatrix),
      })
    }).then(
        response => response.text() // .json(), etc.
        // same as function(response) {return response.text();}
    ).then(html => {
      console.log(html);
      gizmoGui.saved();
    });
  }

  // setup threejs scene with its draw loop running

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

  const updateThreeCamera = (state) => {
    cameraMatrix.fromArray(state.cameraMatrix);
    cameraMatrix.invert();
    cameraMatrix.decompose(threeCamera.position, threeCamera.quaternion, threeCamera.scale);
    threeCamera.aspect = window.innerWidth / window.innerHeight;
    threeCamera.setFocalLength((state.cameraFov * threeCamera.filmGauge / threeCamera.aspect));
    threeCamera.updateProjectionMatrix();
  }

  control = new TransformControls( threeCamera, renderer.domElement );
  control.addEventListener( 'dragging-changed', function ( event ) {
    camera.moveEnabled = ! event.value;
    mouse.moveEnabled = ! event.value;
    if (!event.value) {
      gizmoGui.dirty();
    }
  });
  control.attach( controlObject );
  const gizmo = control.getHelper();
  scene.add( gizmo );
  control.setMode( 'translate' );

  let gizmoGui = createGizmoGui(save, (mode) => control.setMode(mode));

  return {
    toState: () => {
      if ( ! resetGizmo) {
        // Caluclate adjustment matrix from changes to the gizmo
        mat4.multiply(gizmoAdjustmentMatrix, transforms.after, controlObject.matrixWorld.toArray());
        mat4.invert(gizmoAdjustmentMatrix, gizmoAdjustmentMatrix);
      }
      return {
        gizmoAdjustmentMatrix: gizmoAdjustmentMatrix,
      };
    },
    update: (state) => {
      if (resetGizmo) {
        // Find the starting matrix of the gizmo
        transforms = findGizmoTransforms(state);
        mat4.invert(inverseAfter, transforms.after);
  
        controlObjectMatrix.fromArray(inverseAfter);
        controlObjectMatrix.decompose(controlObject.position, controlObject.quaternion, controlObject.scale);
        control.setSpace('local');

        resetGizmo = false;
      }
      updateThreeCamera(state);
    },
    render: () => {
      renderer.render( scene, threeCamera );
    }
  }
}

module.exports = {
  preprocessShader,
  createGizmo,
}
