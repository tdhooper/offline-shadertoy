import { mat4 } from 'gl-matrix';
import * as THREE from 'three';
import { TransformControls } from '../TransformControls';
import createGizmoGui from './gizmo-gui';
import GizmoTransformFinder from './gizmo-transform-finder';
import gizmoShaderModifier from './gizmo-shader-modifier';
import * as pexHelpers from '/lib/pex-helpers';

/* 

TODO: 

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

export default class Gizmo {

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

  transformFinder;
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
    if ( ! gizmoShaderModifier.usesTransformMethod(shader.glsl)) {
      return false;
    }
    shader.glsl = gizmoShaderModifier.addTransformMethodIfMissing(shader.glsl).source;
    shader.glsl = gizmoShaderModifier.makeTransformMethodAdjustable(shader.glsl);
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

    uniforms.gizmoAdjust = pexHelpers.cmdProp('gizmoAdjust');
    uniforms['gizmoAdjustment.t'] = pexHelpers.cmdProp('gizmoAdjustment.t');
    uniforms['gizmoAdjustment.r'] = pexHelpers.cmdProp('gizmoAdjustment.r');
    uniforms['gizmoAdjustment.s'] = pexHelpers.cmdProp('gizmoAdjustment.s');

    let mapNode = this._findMapNode(renderNodes);
    this.shadersHaveMapFunction = mapNode != null;
    
    if ( ! this.shadersHaveMapFunction) {
      console.log('Could not find GIZMO_MAP function');
      return;
    }

    this.camera = camera;
    this.mouse = mouse;

    this.transformFinder = new GizmoTransformFinder(mapNode, uniforms);

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
    ctx.gl.canvas.after(this.renderer.domElement);
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
      let results = this.transformFinder.find(state);

      let workingMat = mat4.create();
      let threeWorkingMat = new THREE.Matrix4();

      // Set the world matrix of the control parent to the same as the map gizmo's parent world
      mat4.invert(workingMat, results.parentTransform);
      threeWorkingMat.fromArray(workingMat);
      threeWorkingMat.decompose(this.controlParent.position, this.controlParent.quaternion, this.controlParent.scale);

      // Set the local matrix of the gizmo object to the same as the map gizmo's local
      let args = results.transformArguments;
      this.controlObject.position.fromArray(args.t);
      let axis = (new THREE.Vector3()).fromArray(args.r);
      this.controlObject.quaternion.setFromAxisAngle(axis, args.r[3])
      this.controlObject.scale.fromArray(args.s);

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
