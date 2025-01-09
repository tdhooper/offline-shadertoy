import { mat4 } from 'gl-matrix';
import * as THREE from 'three';

export default class GizmoControl extends THREE.Object3D {
  
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
