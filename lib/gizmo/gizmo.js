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

class GizmoControl {
  parentObject;
  controlObject;
  adjusted;

  workingMat = mat4.create();
  threeWorkingMat = new THREE.Matrix4();

  constructor(scene) {
    this.parentObject = new THREE.Object3D();
    scene.add(this.parentObject);

    const geometry = new THREE.BoxGeometry( 0, 0, 0 );
    const material = new THREE.MeshBasicMaterial( {
      color: 0x00ff00,
    } );
    this.controlObject = new THREE.Mesh(geometry, material);
    this.parentObject.add(this.controlObject);

    this.adjusted = false;
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

  adjustment() {
    let t = this.controlObject.position.toArray();
    let r = this._quaternionToAxisAngle(this.controlObject.quaternion);
    let s = this.controlObject.scale.toArray();
    return {t, r, s};
  }

  update(parentTransform, transformArguments) {
    // Set the world matrix of the control parent to the same as the map gizmo's parent world
    mat4.invert(this.workingMat, parentTransform);
    this.threeWorkingMat.fromArray(this.workingMat);
    this.threeWorkingMat.decompose(this.parentObject.position, this.parentObject.quaternion, this.parentObject.scale);

    // Set the local matrix of the gizmo object to the same as the map gizmo's local
    let args = transformArguments;
    this.controlObject.position.fromArray(args.t);
    let axis = (new THREE.Vector3()).fromArray(args.r);
    this.controlObject.quaternion.setFromAxisAngle(axis, args.r[3])
    this.controlObject.scale.fromArray(args.s);
  }
}

export default class Gizmo {

  shaderFiles = [];
  gizmoCount;
  ready = false;
  enabled;

  gizmoControls;

  renderer;
  scene;
  threeCamera;
  control;
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
    let result;
    result = gizmoShaderModifier.addTransformMethodIfMissing(shader.glsl);
    result = gizmoShaderModifier.addIndiciesToTransformMethodCalls(result.source);
    this.gizmoCount = result.count;
    result = gizmoShaderModifier.makeTransformMethodAdjustable(result.source, result.count);
    shader.glsl = result.source;
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

    uniforms['gizmoAdjust[0]'] = pexHelpers.cmdProp('gizmoAdjust');
    uniforms['gizmoAdjustment[0].t'] = pexHelpers.cmdProp('gizmoAdjustment.t');
    uniforms['gizmoAdjustment[0].r'] = pexHelpers.cmdProp('gizmoAdjustment.r');
    uniforms['gizmoAdjustment[0].s'] = pexHelpers.cmdProp('gizmoAdjustment.s');

    let mapNode = this._findMapNode(renderNodes);
    this.shadersHaveMapFunction = mapNode != null;
    
    if ( ! this.shadersHaveMapFunction) {
      console.log('Could not find GIZMO_MAP function');
      return;
    }

    this.camera = camera;
    this.mouse = mouse;

    this.transformFinder = new GizmoTransformFinder(mapNode, uniforms, this.gizmoCount);

    this._setupThree();

    this.gizmoControls = [];

    for (let i = 0; i < this.gizmoCount; i++) {
      let gizmoControl = new GizmoControl(this.scene);
      this.gizmoControls.push(gizmoControl);
    }

    this.control.attach(this.gizmoControls[0].controlObject);

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
    
    this.control = new TransformControls(this.threeCamera, this.renderer.domElement );
    this.control.addEventListener( 'dragging-changed', this._threeGizmoDraggingChanged.bind(this));
    const gizmo = this.control.getHelper();
    this.scene.add( gizmo );
    this.control.setMode( 'translate' );

    this.cameraMatrix = new THREE.Matrix4();
  }

  _threeGizmoDraggingChanged(event) {
    this.gizmoControls[0].adjusted = true;
    this.camera.moveEnabled = ! event.value;
    this.mouse.moveEnabled = ! event.value;
    if (!event.value) {
      this.gui.dirty();
    }
  }

  _save() {

    let data = {
      'files': this.shaderFiles,
      'gizmoAdjustment': this.gizmoControls[0].adjustment(),
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

    let state = {
      gizmoAdjust: this.gizmoControls[0].adjusted,
      gizmoAdjustment: this.gizmoControls[0].adjustment(),
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

      this.gizmoControls[0].update(results.parentTransform, results.transformArguments);

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
