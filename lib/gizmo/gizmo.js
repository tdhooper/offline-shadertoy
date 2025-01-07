import { mat4 } from 'gl-matrix';
import * as THREE from 'three';
import { TransformControls } from '../TransformControls';
import createGizmoGui from './gizmo-gui';
import GizmoTransformFinder from './gizmo-transform-finder';
import gizmoShaderModifier from './gizmo-shader-modifier';
import * as pexHelpers from '/lib/pex-helpers';

class GizmoControl extends THREE.Object3D {
  
  initialised = false;

  parentObject;
  controlObject;
  selectObject;
  adjusted;
  camera;
  _active;
  _mouseOver = false;
  mouseDown = false;

  workingMat = mat4.create();
  threeWorkingMat = new THREE.Matrix4();
  cameraWorldPosition = new THREE.Vector3();
  worldPosition = new THREE.Vector3();
  raycaster = new THREE.Raycaster();
  pointer = new THREE.Vector2();

  constructor(camera) {

    super();

    this.camera = camera;

    this.parentObject = new THREE.Object3D();
    this.add(this.parentObject);

    this.controlObject = new THREE.Object3D();
    this.parentObject.add(this.controlObject);

    const geometry = new THREE.SphereGeometry(.05);
    const material = new THREE.MeshBasicMaterial( {
      color: 0x0098ff,
    } );
    this.selectObject = new THREE.Mesh(geometry, material);
    this.add(this.selectObject);

    this.addEventListener('mouseenter', () => {
      this.selectObject.material.color.set(0x23dbff);
    });

    this.addEventListener('mouseleave', () => {
      this.selectObject.material.color.set(0x0098ff);
    });

    this.selectAreaObject = new THREE.Mesh(geometry, material);
    this.selectAreaObject.scale.set(3, 3, 3);
    this.selectAreaObject.visible = false;
    this.selectObject.add(this.selectAreaObject);

    this.adjusted = false;
    this.active = false;

    window.addEventListener('pointermove', this._onPointerMove.bind(this));
  }

  _onPointerMove(event) {
    if (this.active) {
      return;
    }
    this.pointer.x = ( event.clientX / window.innerWidth ) * 2 - 1;
    this.pointer.y = - ( event.clientY / window.innerHeight ) * 2 + 1;
    this.raycaster.setFromCamera(this.pointer, this.camera );
    const intersections = this.raycaster.intersectObject(this.selectObject);
    const intersects = intersections.length > 0;
    this.mouseOver = intersects;
  }

  get mouseOver() {
    return this._mouseOver;
  }

  set mouseOver(value) {
    if (value != this._mouseOver) {
      this._mouseOver = value;
      if (value) {
        this.dispatchEvent({type: 'mouseenter'});
      } else {
        this.dispatchEvent({type: 'mouseleave'});
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
      if (value) {
        this.mouseOver = false;
      }
    }
  }

  updateMatrixWorld(force) {
    this.camera.getWorldPosition(this.cameraWorldPosition);
    this.controlObject.getWorldPosition(this.worldPosition);
    const factor = this.worldPosition.distanceTo( this.cameraWorldPosition ) * Math.min( 1.9 * Math.tan( Math.PI * this.camera.fov / 360 ) / this.camera.zoom, 7 );
    const size = 700 / window.innerHeight;
    this.selectObject.scale.set( 1, 1, 1 ).multiplyScalar( factor * size / 4 );
    this.selectObject.position.copy(this.worldPosition);
    super.updateMatrixWorld(force);
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

  updateParentTransform(parentTransform) {
    // Set the world matrix of the control parent to the same as the map gizmo's parent world
    mat4.invert(this.workingMat, parentTransform);
    this.threeWorkingMat.fromArray(this.workingMat);
    this.threeWorkingMat.decompose(this.parentObject.position, this.parentObject.quaternion, this.parentObject.scale);
  }

  updateTransformArguments(transformArguments) {
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
      gizmoControl.addEventListener('mouseenter', this._mouseEnterElement.bind(this));
      gizmoControl.addEventListener('mouseleave', this._mouseLeaveElement.bind(this));
      this.scene.add(gizmoControl);
      this.gizmoControls.push(gizmoControl);
    }

    this.gui = createGizmoGui(this._save.bind(this), (mode) => this.control.setMode(mode));
  
    this.enabled = document.body.matches(':hover');
    document.addEventListener('mouseleave', this._disable.bind(this));
    document.addEventListener('mouseenter', this._enable.bind(this));

    this.renderer.domElement.addEventListener('pointerdown', this._onPointerDown.bind(this));
    this.renderer.domElement.addEventListener('pointerup', this._onPointerUp.bind(this));
  
    this.initialised = true;
  }

  _onPointerDown() {
    for(let index = 0; index < this.gizmoControls.length; index++) {
      let gizmoControl = this.gizmoControls[index];
      if (gizmoControl.mouseOver) {
        gizmoControl.mouseDown = true;
        break;
      }
    }
  }

  _onPointerUp() {
    for(let index = 0; index < this.gizmoControls.length; index++) {
      let gizmoControl = this.gizmoControls[index];
      if (gizmoControl.mouseOver && gizmoControl.mouseDown) {
        this._setActiveGizmo(index);
        break;
      }
    }
    for(let index = 0; index < this.gizmoControls.length; index++) {
      let gizmoControl = this.gizmoControls[index];
      gizmoControl.mouseDown = false;
    }
  }

  _setActiveGizmo(activeIndex) {
    this.control.attach(this.gizmoControls[activeIndex].controlObject);
    this.control.getHelper().updateMatrixWorld(true); // ensure its intersection tests work immediately
    this.control.pointerHover(null);

    this.activeGizmo = activeIndex;
    this.gizmoControls[activeIndex].active = true;

    this.gizmoControls.forEach((gizmoControl, index) => {
      gizmoControl.active = index === activeIndex;
    })
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
    this.control.addEventListener( 'objectChange', this._threeGizmoChanged.bind(this));
    this.control.addEventListener( 'axis-changed', this._threeGizmoAxisChanged.bind(this));

    const gizmo = this.control.getHelper();
    this.scene.add( gizmo );
    this.control.setMode( 'translate' );

    this.cameraMatrix = new THREE.Matrix4();
  }

  _threeGizmoChanged(event) {
    this.gizmoControls[this.activeGizmo].adjusted = true;
    if (!event.value) {
      this.gui.dirty();
    }
  }

  lastAxis = null;

  _threeGizmoAxisChanged(event) {
    if (this.lastAxis != event.value) {
      if (this.lastAxis == null) {
        this._mouseEnterElement();
      } else if (event.value == null) {
        this._mouseLeaveElement();
      }
      this.lastAxis = event.value;
    }
  }

  mouseOverElements = 0;

  _mouseEnterElement() {
    this.mouseOverElements++;
    this.mouse.moveEnabled = this.mouseOverElements <= 0;
    this.camera.moveEnabled = this.mouseOverElements <= 0;
  }

  _mouseLeaveElement() {
    this.mouseOverElements--;
    this.mouse.moveEnabled = this.mouseOverElements <= 0;
    this.camera.moveEnabled = this.mouseOverElements <= 0;
  }

  _save() {

    let data = {
      'file': this.shaderFiles[0],
      'gizmoAdjustments': this.gizmoControls.map(
        gizmoControl => gizmoControl.adjusted ? gizmoControl.adjustment() : false
      ),
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
      this.gui.saved();
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
    
    if ( ! this.initialised || ! this.shadersHaveGizmoFunction) {
      return {};
    }

    let state = {
      gizmoAdjust: this.gizmoControls.map(gizmoControl => gizmoControl.adjusted),
      gizmoAdjustment: this.gizmoControls.map(gizmoControl => gizmoControl.adjustment()),
    };

    return state;
  }

  update(state) {

    if ( ! this.initialised || ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction) {
      return;
    }

    if ( ! this.ready) {
      this._updateGizmos(state);
      this.control.setSpace('local');
      this.ready = true;
    }

    if (this.control.dragging) {
      this._updateGizmos(state);
    }

    this._updateThreeCamera(state);
  }

  _updateGizmos(state) {
    let results = this.transformFinder.find(state);
    for (let i = 0; i < this.gizmoCount; i++) {
      if (this.control.dragging && this.activeGizmo == i) {
        continue;
      }
      this.gizmoControls[i].updateParentTransform(results.gizmosParentTransform[i]);
      if ( ! this.gizmoControls[i].adjusted) {
        this.gizmoControls[i].updateTransformArguments(results.gizmosTransformArguments[i]);
      }
    }
  }

  render() {
    if ( ! this.initialised || ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction || ! this.enabled) {
      return;
    }

    this.control.size = 700 / window.innerHeight;

    this.renderer.render(this.scene, this.threeCamera);
  }
}
