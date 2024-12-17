const { mat4, vec3, quat } = require('gl-matrix');
const THREE = require('three');
const { TransformControls } = require('./TransformControls');
const createGizmoGui = require('./gizmo-gui');


class GizmoOriginFinder {

  evalGizmoPositions;
  evalGizmoResults;
  _draw;

  constructor(mapNode, uniforms) {

    this.evalGizmoPositions = regl.texture({
      width: 10,
      height: 2,
      format: 'rgb',
      type: 'float'
    });
    
    this.evalGizmoResults = regl.framebuffer({
      width: 10,
      height: 2,
      colorType: 'float',
    });

    this._draw = this._createDraw(mapNode, uniforms);
  }

  _createDraw(mapNode, uniforms) {
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
      evalGizmoPositions: this.evalGizmoPositions,
      evalGizmoPositionsResolution: (context, props) => {
        return [this.evalGizmoPositions.width, this.evalGizmoPositions.height];
      },
    }, uniforms);

    let reglDraw = regl({
      frag: shader,
      uniforms: gizmoUniforms,
      framebuffer: this.evalGizmoResults,
    });

    return (state) => {
      mapNode.draw(state, () => {
        reglDraw(state);
      });
    };
  }

  _configure(positions) {

    // Update input buffer data
    let dataA = positions[0].reduce((acc, p) => {return acc.concat([p[0], p[1], p[2]])}, []);
    let dataB = positions[1].reduce((acc, p) => {return acc.concat([p[0], p[1], p[2]])}, []);
    let data = [].concat(dataA, dataB);
    this.evalGizmoPositions({
      width: positions[0].length,
      height: positions.length,
      format: 'rgb',
      type: 'float',
      data: data,
    });

    // Set results framebuffer size
    if (this.evalGizmoResults.width != positions.length) {
      this.evalGizmoResults.resize(this.evalGizmoPositions.width, this.evalGizmoPositions.height);
    }

    // Clear results buffer
    regl.clear({
      color: [0,0,0,1],
      depth: 1,
      stencil: 0,
      framebuffer: this.evalGizmoResults,
    });
  }

  _vec3FromArray(array, index) {
    return new Float32Array(array.buffer, index * 16, 3);
  }

  _findOriginTransformsFromSourcePoints(state, points) {

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

    this._configure([positions, positions]);
    this._draw(state);

    let count = worldMatrices.length;
    var bytes = new Float32Array(count * 4 * 2 * 4);

    regl.read({
      framebuffer: this.evalGizmoResults,
      data: bytes,
    });

    let transformsBefore = [];
    let transformsAfter = [];

    for (let i = 0; i < count * 2; i++) {

      let x = this._vec3FromArray(bytes, i * 4 + 0);
      let y = this._vec3FromArray(bytes, i * 4 + 1);
      let z = this._vec3FromArray(bytes, i * 4 + 2);
      let o = this._vec3FromArray(bytes, i * 4 + 3);

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

  find(state) {
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

    let transforms = this._findOriginTransformsFromSourcePoints(state, points);

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
}

class Gizmo {

  shaderFiles = [];
  gizmoAdjustmentMatrix;
  firstUpdate = true;
  enabled;
  rendered = false;

  renderer;
  scene;
  threeCamera;
  control;
  controlObject;
  camera;
  mouse;

  originFinder;
  inverseAfter;
  combinedAdjustmentMatrix;
  cameraMatrix;
  controlObjectMatrix;
  transforms;

  shadersHaveGizmoFunction;
  shadersHaveMapFunction;

  preprocessShaders(shaders) {
    shaders.forEach((shader) => {
      if (this._preprocessShader(shader)) {
        this.shaderFiles.push(shader.file);
      }
    });

    this.shadersHaveGizmoFunction = this.shaderFiles.length != 0;

    if ( ! this.shadersHaveGizmoFunction) {
      console.log('no GIZMO functions in use');
    }
  }

  _preprocessShader(shader) {

    if (shader.glsl.indexOf('GIZMO(') == -1) {
      return false;
    }
    
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

    return true;
  }

  _findMapNode(renderNodes) {
    for (let i = renderNodes.length - 1; i >= 0; i--) {
      let node = renderNodes[i];
      if (node.shader.indexOf('GIZMO_MAP(') != -1) {
        return node;
      }
    }
  }

  initialise(camera, mouse, renderNodes, uniforms) {
    
    if ( ! this.shadersHaveGizmoFunction) {
      return;
    }

    this.gizmoAdjustmentMatrix = mat4.create();
    uniforms.gizmoAdjustmentMatrix = regl.prop('gizmoAdjustmentMatrix');

    let mapNode = this._findMapNode(renderNodes);
    this.shadersHaveMapFunction = mapNode != null;
    
    if ( ! this.shadersHaveMapFunction) {
      console.log('Could not find GIZMO_MAP function');
      return;
    }

    this.camera = camera;
    this.mouse = mouse;

    this.originFinder = new GizmoOriginFinder(mapNode, uniforms);

    this.inverseAfter = mat4.create();
    this.combinedAdjustmentMatrix = mat4.create();

    this._setupThree();

    this.gui = createGizmoGui(this._save.bind(this), (mode) => this.control.setMode(mode));
  
    this.enabled = document.body.matches(':hover');
    document.addEventListener('mouseleave', this._disable.bind(this));
    document.addEventListener('mouseenter', this._enable.bind(this));
  }

  _disable() {
    this.enabled = false;
    this.renderer.domElement.style.display = 'none';
  }

  _enable() {
    this.enabled = true;
    this.renderer.domElement.style.display = 'block';
  }

  _setupThree() {
    // setup threejs scene with its draw loop running

    this.scene = new THREE.Scene();
    this.threeCamera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.01, 1000 );
    this.renderer = new THREE.WebGLRenderer({
      alpha: true
    });
    this.renderer.setSize(window.innerWidth, window.innerHeight);
    regl._gl.canvas.after(this.renderer.domElement);
    this.renderer.domElement.style.position = 'absolute';
    window.addEventListener('resize', () => {
      this.renderer.setSize(window.innerWidth, window.innerHeight);
    });
    
    const geometry = new THREE.BoxGeometry( 0, 0, 0 );
    const material = new THREE.MeshBasicMaterial( {
      color: 0x00ff00,
    } );

    this.controlObject = new THREE.Mesh(geometry, material);
    this.scene.add(this.controlObject);

    this.control = new TransformControls(this.threeCamera, this.renderer.domElement );
    this.control.addEventListener( 'dragging-changed', this._threeGizmoDraggingChanged.bind(this));
    this.control.attach(this.controlObject);
    const gizmo = this.control.getHelper();
    this.scene.add( gizmo );
    this.control.setMode( 'translate' );

    this.cameraMatrix = new THREE.Matrix4();
    this.controlObjectMatrix = new THREE.Matrix4();
  }

  _threeGizmoDraggingChanged(event) {
    this.camera.moveEnabled = ! event.value;
    this.mouse.moveEnabled = ! event.value;
    if (!event.value) {
      this.gui.dirty();
    }
  }

  _save() {
    mat4.multiply(this.combinedAdjustmentMatrix, this.transforms.before, this.controlObject.matrixWorld.toArray());
    mat4.invert(this.combinedAdjustmentMatrix, this.combinedAdjustmentMatrix);

    fetch('/save-gizmo', {
      method : "POST",
      headers: {
        'Content-Type': 'application/json',
      },
      body : JSON.stringify({
        'files': this.shaderFiles,
        'matrix': Array.from(this.combinedAdjustmentMatrix),
      })
    }).then(
        response => response.text() // .json(), etc.
        // same as function(response) {return response.text();}
    ).then(html => {
      console.log(html);
      this.gizmoGui.saved();
    });
  }

  _updateThreeCamera = (state) => {
    this.cameraMatrix.fromArray(state.cameraMatrix);
    this.cameraMatrix.invert();
    this.cameraMatrix.decompose(this.threeCamera.position, this.threeCamera.quaternion, this.threeCamera.scale);
    this.threeCamera.aspect = window.innerWidth / window.innerHeight;
    this.threeCamera.setFocalLength((state.cameraFov * this.threeCamera.filmGauge / this.threeCamera.aspect));
    this.threeCamera.updateProjectionMatrix();
  }

  toState() {
    
    if ( ! this.shadersHaveGizmoFunction) {
      return {};
    }

    if ( ! this.firstUpdate && this.enabled && this.rendered) {
      // Caluclate adjustment matrix from changes to the gizmo
      mat4.multiply(this.gizmoAdjustmentMatrix, this.transforms.after, this.controlObject.matrixWorld.toArray());
      mat4.invert(this.gizmoAdjustmentMatrix, this.gizmoAdjustmentMatrix);
    }

    return {
      gizmoAdjustmentMatrix: this.gizmoAdjustmentMatrix,
    };
  }

  update(state) {

    if ( ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction) {
      return;
    }
    
    if (this.firstUpdate) {
      // Find the starting matrix of the gizmo
      this.transforms = this.originFinder.find(state);
      mat4.invert(this.inverseAfter, this.transforms.after);

      this.controlObjectMatrix.fromArray(this.inverseAfter);
      this.controlObjectMatrix.decompose(this.controlObject.position, this.controlObject.quaternion, this.controlObject.scale);
      this.control.setSpace('local');

      this.firstUpdate = false;
    }

    this._updateThreeCamera(state);
  }

  render() {

    if ( ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction) {
      return;
    }
    
    if (this.enabled) {
      this.renderer.render(this.scene, this.threeCamera);
      this.rendered = true;
    }
  }
}

module.exports = Gizmo;
