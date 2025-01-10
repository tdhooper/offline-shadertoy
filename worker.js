import GizmoWorker from './lib/gizmo/gizmo-worker';
import createRenderer from './renderer';
import * as Comlink from 'comlink';

let gizmoWorker;
let renderer;

const obj = {
  start (options) {
    gizmoWorker = new GizmoWorker();
    renderer = createRenderer(options.project, options.offscreenCanvas, gizmoWorker);
  },
  gizmoCount() {
    return gizmoWorker.gizmoCount;
  },
  shaderFiles() {
    return gizmoWorker.shaderFiles;
  },
  gizmoInitialise() {
    return gizmoWorker.initialise();
  },
  gizmoFindTransforms(state) {
    return gizmoWorker.findTransforms(state);
  },
  rendererDraw(state, done) {
    return renderer.draw(state, done);
  },
  rendererResize(width, height) {
    return renderer.resize(width, height);
  },
};

Comlink.expose(obj);

