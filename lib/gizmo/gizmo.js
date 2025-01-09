import * as THREE from 'three';
import { TransformControls } from '../TransformControls';
import createGizmoGui from './gizmo-gui';
import GizmoControl from './gizmo-control';

export default class Gizmo {

  gizmoWorker;
  initialised = false;

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

  preInitialise(gizmoCount) {
    this.gizmoCount = gizmoCount;
  }

  initialise(camera, mouse, gizmoWorker) {

    gizmoWorker.initialise();

    if ( ! gizmoWorker.initialised) {
      return;
    }

    this.gizmoWorker = gizmoWorker;
    this.shaderFiles = gizmoWorker.shaderFiles;

    this.camera = camera;
    this.mouse = mouse;

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

    if ( ! this.initialised ) {
      return {
        gizmoAdjust: (new Array(this.gizmoCount)).fill(false),
        gizmoAdjustment: (new Array(this.gizmoCount)).fill({t: [0,0,0], r: [1,0,0,0], s: [1,1,1]}),
      };
    }

    let state = {
      gizmoAdjust: this.gizmoControls.map(gizmoControl => gizmoControl.adjusted),
      gizmoAdjustment: this.gizmoControls.map(gizmoControl => gizmoControl.adjustment()),
    };

    return state;
  }

  update(state) {

    if ( ! this.initialised ) {
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

    this.gizmoWorker._updateGizmos(state);
    let results = this.gizmoWorker.transformFinderResults;

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
    if ( ! this.initialised || ! this.enabled) {
      return;
    }

    this.control.size = 700 / window.innerHeight;

    this.renderer.render(this.scene, this.threeCamera);
  }
}
