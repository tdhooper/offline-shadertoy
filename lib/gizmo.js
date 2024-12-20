const { mat4, vec3, quat } = require('gl-matrix');
const THREE = require('three');
const { TransformControls } = require('./TransformControls');
const createGizmoGui = require('./gizmo-gui');

/* 

TODO: 

* Actually write egTransform into the shader file
  * Rename it to gTransform
  * preprocessShader
    * add adjustment uniforms
    * add/replace with gTransform that uses adjustments
  * GizmoOriginFinder
    * add globals
    * remove adjustment uniforms (optional, they won't get used)
    * add/replace with gTransform that writes data to globals
  * On Save
    * check if it already exists, write it to file

* Allow writing file without causing a reload and recompile
  * we probably need to switch to vite:
    * https://github.com/szymmis/vite-express
    * https://github.com/axe-me/vite-plugin-node
    * https://github.com/bluwy/create-vite-extra/tree/master/template-ssr-vanilla

* Support multiple gizmos
  * preprocessShader
    * change adjustment uniforms to be an array
    * add index to calls: gTransform(3, ...)
    * change gTransform to read appropriate adjustment data for the index
  * GizmoOriginFinder
    * count the number of gTransform instances
    * change globals to be arrays
    * change gTransform to write to the appropriate slot in the global array
    * fetch data for each gTransform into a separate row of the result texture
  * On Save
    * include index with data sent to server
    * only update the changed gizmos
  * Fetch new gizmo transforms on move - as moving one gizmo might affect the
    parent transform of others

*/


class GizmoOriginFinder {

  evalGizmoPositions;
  evalGizmoResults;
  _draw;

  constructor(mapNode, uniforms) {

    this.evalGizmoPositions = regl.texture({
      width: 10,
      height: 1,
      format: 'rgb',
      type: 'float'
    });
    
    this.evalGizmoResults = regl.framebuffer({
      width: 10,
      height: 1,
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

        if (gl_FragCoord.x < 1.) {
          gl_FragColor = vec4(GIZMO_ARGUMENTS.t, 0);
        } else if (gl_FragCoord.x < 2.) {
          gl_FragColor = GIZMO_ARGUMENTS.r;
        } else if (gl_FragCoord.x < 3.) {
          gl_FragColor = vec4(GIZMO_ARGUMENTS.s, 0);
        } else if (gl_FragCoord.x < 4.) {
          gl_FragColor = vec4(GIZMO_PARENT_P, 0);
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
      state = Object.assign({}, state);
      state.gizmoAdjust = false;
      mapNode.draw(state, () => {
        reglDraw(state);
      });
    };
  }

  _configure(positions) {

    // Update input buffer data
    let emptyData = Array(3 * 3).fill(0);
    let positionsData = positions.reduce((acc, p) => {return acc.concat([p[0], p[1], p[2]])}, []);
    let data = [].concat(emptyData, positionsData);
    this.evalGizmoPositions({
      width: data.length / 3,
      height: 1,
      format: 'rgb',
      type: 'float',
      data: data,
    });

    // Set results framebuffer size
    if (this.evalGizmoResults.width != this.evalGizmoPositions.width) {
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

  _vec4FromArray(array, index) {
    return new Float32Array(array.buffer, index * 16, 4);
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

    this._configure(positions);
    this._draw(state);

    let cols = this.evalGizmoResults.width;
    let rows = this.evalGizmoResults.height;
    var bytes = new Float32Array(cols * rows * 4);

    regl.read({
      framebuffer: this.evalGizmoResults,
      data: bytes,
    });

    let t = this._vec3FromArray(bytes, 0);
    let r = this._vec4FromArray(bytes, 1);
    let s = this._vec3FromArray(bytes, 2);

    let parentTransforms = [];
    let argumentsCount = 3;

    for (let i = 0; i < points.length; i++) {

      let x = this._vec3FromArray(bytes, argumentsCount + i * 4 + 0);
      let y = this._vec3FromArray(bytes, argumentsCount + i * 4 + 1);
      let z = this._vec3FromArray(bytes, argumentsCount + i * 4 + 2);
      let o = this._vec3FromArray(bytes, argumentsCount + i * 4 + 3);

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
      mat4.invert(inverseWorld, worldMatrices[i]);
      mat4.multiply(local, local, inverseWorld);
      //mat4.invert(local, local);

      parentTransforms.push(local);
    }

    return {
      parentTransforms: parentTransforms,
      transformArguments: {t, r, s},
    };
  }

  find(state) {
    //*
    let points = [
      vec3.fromValues( 0,  0,  0),
    ];

    let size = 4;
    let count = 4;
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

    let results = this._findOriginTransformsFromSourcePoints(state, points);

    let lowestScore = Infinity;
    let lowestScoreIndex = 0;
    let difference = mat4.create();

    results.parentTransforms.forEach((transform, i) => {

      let score = results.parentTransforms.reduce((totalScore, otherTransform) => {
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
      parentTransform: results.parentTransforms[lowestScoreIndex],
      transformArguments: results.transformArguments,
    };
  }
}

class Gizmo {

  shaderFiles = [];
  adjustmentObj;
  ready = false;
  enabled;
  adjusted = false;

  renderer;
  scene;
  threeCamera;
  control;
  controlParent;
  controlObject;
  camera;
  mouse;

  originFinder;
  cameraMatrix;

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

    if (shader.glsl.indexOf('egTransform(') == -1) {
      return false;
    }
    
    shader.glsl = `
      precision mediump float;
  
      struct GizmoTransform {
        vec3 t;
        vec4 r;
        vec3 s;
      };

      uniform bool gizmoAdjust;
      uniform GizmoTransform gizmoAdjustment;
  
      vec3 GIZMO_PARENT_P;
      GizmoTransform GIZMO_ARGUMENTS;
      bool GIZMO_SET = false;

      float gTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
          p += t;
          p = mix(dot(r.xyz,p)*r.xyz, p, cos(r.w))+sin(r.w)*cross(r.xyz,p);
          p /= s;
          return min(s.x, min(s.y, s.z));
      }

      float egTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
          if ( ! GIZMO_SET) GIZMO_PARENT_P = p;
          float scale;
          if (gizmoAdjust) {
              scale = gTransform(p, gizmoAdjustment.t, gizmoAdjustment.r, gizmoAdjustment.s);
          } else {
              scale = gTransform(p, t, r, s);
          }
          if ( ! GIZMO_SET) GIZMO_ARGUMENTS = GizmoTransform(t, r, s);
          GIZMO_SET = true;
          return scale;
      }

      float egTransform(inout vec3 p) {
          if ( ! GIZMO_SET) GIZMO_PARENT_P = p;
          if ( ! GIZMO_SET) GIZMO_ARGUMENTS = GizmoTransform(vec3(0, 0, 0), vec4(1, 0, 0, 0), vec3(1, 1, 1));
          float scale = gTransform(p, gizmoAdjustment.t, gizmoAdjustment.r, gizmoAdjustment.s);
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

    uniforms.gizmoAdjust = regl.prop('gizmoAdjust');
    uniforms['gizmoAdjustment.t'] = regl.prop('gizmoAdjustment.t');
    uniforms['gizmoAdjustment.r'] = regl.prop('gizmoAdjustment.r');
    uniforms['gizmoAdjustment.s'] = regl.prop('gizmoAdjustment.s');

    let mapNode = this._findMapNode(renderNodes);
    this.shadersHaveMapFunction = mapNode != null;
    
    if ( ! this.shadersHaveMapFunction) {
      console.log('Could not find GIZMO_MAP function');
      return;
    }

    this.camera = camera;
    this.mouse = mouse;

    this.originFinder = new GizmoOriginFinder(mapNode, uniforms);

    this._setupThree();

    this.adjustmentObj = this._adjustmentObj(new THREE.Object3D());

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

    this.controlParent = new THREE.Object3D();
    this.scene.add(this.controlParent);

    this.controlObject = new THREE.Mesh(geometry, material);
    this.controlParent.add(this.controlObject);

    this.control = new TransformControls(this.threeCamera, this.renderer.domElement );
    this.control.addEventListener( 'dragging-changed', this._threeGizmoDraggingChanged.bind(this));
    this.control.attach(this.controlObject);
    const gizmo = this.control.getHelper();
    this.scene.add( gizmo );
    this.control.setMode( 'translate' );

    this.cameraMatrix = new THREE.Matrix4();
  }

  _threeGizmoDraggingChanged(event) {
    this.adjusted = true;
    this.camera.moveEnabled = ! event.value;
    this.mouse.moveEnabled = ! event.value;
    if (!event.value) {
      this.gui.dirty();
    }
  }

  _quaternionToAxisAngle(quat) {
    if (quat.w > 1) quat.normalise(); // if w>1 acos and sqrt will produce errors, this cant happen if quaternion is normalised
    let angle = 2 * Math.acos(quat.w);
    let s = Math.sqrt(1-quat.w*quat.w);
    if (s == 0) {
      return [1, 0, 0, angle];
    } else {
      return [quat.x / s, quat.y / s, quat.z / s, angle];
    }
  }

  _adjustmentObj(obj) {
    let t = obj.position.toArray();
    let r = this._quaternionToAxisAngle(obj.quaternion);
    let s = obj.scale.toArray();

    t[0] *= -1;
    t[1] *= -1;
    t[2] *= -1;

    r[3] *= -1;

    //s[0] = 1 / s[0];
    //s[1] = 1 / s[1];
    //s[2] = 1 / s[2];
    
    return {t, r, s};
  }

  _save() {

    let data = {
      'files': this.shaderFiles,
      'gizmoAdjustment': this._adjustmentObj(this.controlObject),
    };

    fetch('/save-gizmo', {
      method : "POST",
      headers: {
        'Content-Type': 'application/json',
      },
      body : JSON.stringify(data)
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

    if (this.adjusted) {
      this.adjustmentObj = this._adjustmentObj(this.controlObject);
    }
    
    let state = {
      gizmoAdjust: this.adjusted,
      gizmoAdjustment: this.adjustmentObj
    };

    return state;
  }

  update(state) {

    if ( ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction) {
      return;
    }
    
    if ( ! this.ready) {
      // Find the starting matrix of the gizmo
      let results = this.originFinder.find(state);

      let workingMat = mat4.create();
      let threeWorkingMat = new THREE.Matrix4();

      // Set the world matrix of the control parent to the same as the map gizmo's parent world
      mat4.invert(workingMat, results.parentTransform);
      threeWorkingMat.fromArray(workingMat);
      threeWorkingMat.decompose(this.controlParent.position, this.controlParent.quaternion, this.controlParent.scale);

      // Set the local matrix of the gizmo object to the same as the map gizmo's local
      let args = results.transformArguments;
      this.controlObject.position.set(-args.t[0], -args.t[1], -args.t[2]);
      let axis = new THREE.Vector3(args.r[0], args.r[1], args.r[2]);
      this.controlObject.quaternion.setFromAxisAngle(axis, -args.r[3])
      this.controlObject.scale.set(args.s[0], args.s[1], args.s[2]);

      this.control.setSpace('local');

      this.ready = true;
    }

    this._updateThreeCamera(state);
  }

  render() {

    if ( ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction) {
      return;
    }
    
    if (this.enabled) {
      this.renderer.render(this.scene, this.threeCamera);
    }
  }
}

module.exports = Gizmo;
