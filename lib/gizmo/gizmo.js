import * as THREE from 'three';
import { TransformControls } from '../TransformControls';
import createGizmoGui from './gizmo-gui';
import GizmoControl from './gizmo-control';
import GizmoTransformFinder from './gizmo-transform-finder';
import { InteractionObject } from '../interaction-manager';

export default class Gizmo {

  container;
  interactionManager;
  initialised = false;

  shaderFiles;
  gizmoCount;
  mapNode;

  ready = false;
  enabled;

  activeGizmo;
  gizmoControls;

  renderer;
  scene;
  threeCamera;
  transformControl;
  camera;
  mouse;

  transformFinder;
  cameraMatrix;

  constructor(gizmoRendererHooks, container, interactionManager) {
    this.gizmoCount = gizmoRendererHooks.gizmoCount;
    this.shaderFiles = gizmoRendererHooks.shaderFiles;
    this.mapNode = gizmoRendererHooks.mapNode;
    this.container = container;
    this.interactionManager = interactionManager;
  }

  async initialise(camera, mouse) {

    if ( ! this.mapNode || ! this.gizmoCount) {
      return;
    }

    this.transformFinder = new GizmoTransformFinder(this.mapNode, this.gizmoCount);

    this.camera = camera;
    this.mouse = mouse;

    this._setupThree();

    this.gizmoControls = [];

    for (let i = 0; i < this.gizmoCount; i++) {
      let gizmoControl = new GizmoControl(this.threeCamera);
      this.interactionManager.add(gizmoControl.interactionObject);
      gizmoControl.interactionObject.addEventListener('pointerUp', this._setActiveGizmo.bind(this, i));
      this.scene.add(gizmoControl);
      this.gizmoControls.push(gizmoControl);
    }

    this.gui = createGizmoGui(this._save.bind(this), (mode) => this.transformControl.setMode(mode));
  
    this.enabled = document.body.matches(':hover');
    document.addEventListener('mouseleave', this._disable.bind(this));
    document.addEventListener('mouseenter', this._enable.bind(this));
  
    this.initialised = true;
  }

  _setActiveGizmo(activeIndex) {
    this.transformControl.attach(this.gizmoControls[activeIndex].controlObject);
    this.transformControl.getHelper().updateMatrixWorld(true); // ensure its intersection tests work immediately
    this.transformControl.pointerHover(null);

    this.activeGizmo = activeIndex;

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
    this.container.append(this.renderer.domElement);
    this.renderer.domElement.style.position = 'absolute';
    window.addEventListener('resize', () => {
      this.renderer.setSize(window.innerWidth, window.innerHeight);
    });
    
    this.transformControl = new TransformControls(this.threeCamera, this.renderer.domElement );
    this.transformControl.addEventListener( 'objectChange', this._threeGizmoChanged.bind(this));

    const helper = this.transformControl.getHelper();
    this.scene.add( helper );
    this.transformControl.setMode( 'translate' );

    const interactionObject = new InteractionObject(() => {
      return this.transformControl.axis !== null ? 1 : -1; // on top of everything when it has an axis hoevered
    });
    this.interactionManager.add(interactionObject);

    // const geometry = new THREE.SphereGeometry(.5);
    // const material = new THREE.MeshBasicMaterial( {
    //   color: 0xff0000,
    // } );
    // const mesh = new THREE.Mesh(geometry, material);
    // this.scene.add(mesh);

    this.cameraMatrix = new THREE.Matrix4();
  }

  _threeGizmoChanged(event) {
    this.gizmoControls[this.activeGizmo].adjusted = true;
    if (!event.value) {
      this.gui.dirty();
    }
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
    this.cameraMatrix.decompose(this.threeCamera.position, this.threeCamera.quaternion, this.threeCamera.scale);
    this.threeCamera.aspect = window.innerWidth / window.innerHeight;
    this.threeCamera.setFocalLength((state.cameraFov * this.threeCamera.filmGauge / this.threeCamera.aspect));
    this.threeCamera.updateProjectionMatrix();
  }

  toState() {

    if ( ! this.gizmoCount) {
      return {};
    }

    if ( ! this.initialised) {
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
      this.transformControl.setSpace('local');
      this.ready = true;
    }

    if (this.transformControl.dragging) {
      this._updateGizmos(state);
    }

    this._updateThreeCamera(state);
  }

  async _updateGizmos(state) {

    const results = this.transformFinder.find(state);

    for (let i = 0; i < this.gizmoCount; i++) {
      if (this.transformControl.dragging && this.activeGizmo == i) {
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

    this.transformControl.size = 700 / window.innerHeight;

    this.renderer.render(this.scene, this.threeCamera);
  }
}
