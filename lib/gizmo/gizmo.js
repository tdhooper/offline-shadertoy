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

class GizmoControl extends THREE.Object3D {
  parentObject;
  controlObject;
  selectObject;
  adjusted;
  camera;
  _active;
  _mouseOver;

  workingMat = mat4.create();
  threeWorkingMat = new THREE.Matrix4();
  cameraWorldPosition = new THREE.Vector3();
  worldPosition = new THREE.Vector3();

  constructor(camera) {

    super();

    this.camera = camera;

    this.parentObject = new THREE.Object3D();
    this.add(this.parentObject);

    this.controlObject = new THREE.Object3D();
    this.parentObject.add(this.controlObject);

    const geometry = new THREE.SphereGeometry(.05);
    const material = new THREE.MeshBasicMaterial( {
      color: 0x0000ff,
    } );
    this.selectObject = new THREE.Mesh(geometry, material);
    this.add(this.selectObject);

    this.selectAreaObject = new THREE.Mesh(geometry, material);
    this.selectAreaObject.scale.set(3, 3, 3);
    this.selectAreaObject.visible = false;
    this.selectObject.add(this.selectAreaObject);

    this.selectObject.onBeforeRender = this._updateSelectObject.bind(this);

    this.adjusted = false;
    this.active = false;
  }

  _updateSelectObject() {
    this.camera.getWorldPosition(this.cameraWorldPosition);
    this.controlObject.getWorldPosition(this.worldPosition);
    let factor = this.worldPosition.distanceTo( this.cameraWorldPosition ) * Math.min( 1.9 * Math.tan( Math.PI * this.camera.fov / 360 ) / this.camera.zoom, 7 );
    this.selectObject.scale.set( 1, 1, 1 ).multiplyScalar( factor * 1 / 4 );
    this.selectObject.position.copy(this.worldPosition);
  }

  get mouseOver() {
    return this._mouseOver;
  }

  set mouseOver(value) {
    if (value != this._mouseOver) {
      this._mouseOver = value;
      if (value) {
        this._onMouseEnter();
      } else {
        this._onMouseLeave();
      }
    }
  }

  get active() {
    return this._active;
  }

  set active(value) {
    if (value != this._active) {
      this._active = value;
      this.selectObject.visible = ! value;
    }
  }

  _onMouseEnter() {
    this.selectObject.material.color.set(0x00ff00);
  }

  _onMouseLeave() {
    this.selectObject.material.color.set(0x0000ff);
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

  activeGizmo;
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

  raycaster = new THREE.Raycaster();
  pointer = new THREE.Vector2();

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

    uniforms['gizmoAdjust[]'] = pexHelpers.cmdProp('gizmoAdjust');
    uniforms['gizmoAdjustment[]{}'] = pexHelpers.cmdProp('gizmoAdjustment');

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
      let gizmoControl = new GizmoControl(this.threeCamera);
      this.scene.add(gizmoControl);
      this.gizmoControls.push(gizmoControl);
    }

    this._setActiveGizmo(0);

    window.addEventListener('pointermove', this._onPointerMove.bind(this));
    window.addEventListener('pointerdown', this._onPointerDown.bind(this));

    this.gui = createGizmoGui(this._save.bind(this), (mode) => this.control.setMode(mode));
  
    this.enabled = document.body.matches(':hover');
    document.addEventListener('mouseleave', this._disable.bind(this));
    document.addEventListener('mouseenter', this._enable.bind(this));
  }

  _onPointerMove(event) {
    this.pointer.x = ( event.clientX / window.innerWidth ) * 2 - 1;
    this.pointer.y = - ( event.clientY / window.innerHeight ) * 2 + 1;
    this.raycaster.setFromCamera(this.pointer, this.threeCamera );
    let intersectsAny = false;
    this.gizmoControls
      .filter(gizmoControl => ! gizmoControl.active)
      .forEach(gizmoControl => {
        const intersections = this.raycaster.intersectObject(gizmoControl.selectObject);
        const intersects = intersections.length > 0;
        intersectsAny = intersectsAny || intersects;
        gizmoControl.mouseOver = intersects;
      });
    this.renderer.domElement.style.cursor = intersectsAny ? 'pointer' : null;
  }

  _onPointerDown() {
    for (let i = 0; i < this.gizmoControls.length; i++) {
      if (this.gizmoControls[i].mouseOver) {
        this._setActiveGizmo(i);
        break;
      }
    };
  }

  _setActiveGizmo(activeIndex) {
    this.gizmoControls.forEach((gizmoControl, index) => {
      gizmoControl.active = index === activeIndex;
    })
    this.activeGizmo = activeIndex;
    this.control.attach(this.gizmoControls[activeIndex].controlObject);
    this.gizmoControls[activeIndex].mouseOver = false;
    this.renderer.domElement.style.cursor = null;
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
      alpha: true,
      antialias: true,
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
    this.gizmoControls[this.activeGizmo].adjusted = true;
    this.camera.moveEnabled = ! event.value;
    this.mouse.moveEnabled = ! event.value;
    if (!event.value) {
      this.gui.dirty();
    }
  }

  _save() {

    let data = {
      'file': this.shaderFiles[0],
      'gizmoAdjustments': this.gizmoControls.map(gizmoControl => gizmoControl.adjustment()),
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
      gizmoAdjust: this.gizmoControls.map(gizmoControl => gizmoControl.adjusted),
      gizmoAdjustment: this.gizmoControls.map(gizmoControl => gizmoControl.adjustment()),
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

      for (let i = 0; i < this.gizmoCount; i++) {
        this.gizmoControls[i].update(results.gizmosParentTransform[i], results.gizmosTransformArguments[i]);
      }

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
