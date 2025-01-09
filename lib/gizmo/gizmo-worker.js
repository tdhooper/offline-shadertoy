import gizmoShaderModifier from './gizmo-shader-modifier';
import * as pexHelpers from '/lib/pex-helpers';
import GizmoTransformFinder from './gizmo-transform-finder';

export default class GizmoWorker {

  initialised = false;
  shaderFiles = [];
  gizmoCount;
  ready = false;

  transformFinder;
  transformFinderResults;

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

  preprocessNodesAndUniforms(renderNodes, uniforms) {

    if ( ! this.shadersHaveGizmoFunction) {
      return;
    }

    this.mapNode = this._findMapNode(renderNodes);
    this.shadersHaveMapFunction = this.mapNode != null;

    uniforms['gizmoAdjust[]'] = pexHelpers.cmdProp('gizmoAdjust');
    uniforms['gizmoAdjustment[]{}'] = pexHelpers.cmdProp('gizmoAdjustment');

    if ( ! this.shadersHaveMapFunction) {
      console.log('Could not find GIZMO_MAP function');
      return;
    }
  }

  initialise() {
    if ( ! this.shadersHaveGizmoFunction ||  ! this.shadersHaveMapFunction) {
      return;
    }

    this.transformFinder = new GizmoTransformFinder(this.mapNode, this.gizmoCount);
    this.initialised = true;
  }

  _findMapNode(renderNodes) {
    for (let i = renderNodes.length - 1; i >= 0; i--) {
      let node = renderNodes[i];
      if (node.shader.indexOf('GIZMO_MAP(') != -1) {
        return node;
      }
    }
  }

  update(state, isDragging) {

    if ( ! this.initialised || ! this.shadersHaveGizmoFunction || ! this.shadersHaveMapFunction) {
      return;
    }

    if ( ! this.ready) {
      this._updateGizmos(state);
      this.ready = true;
    }

    if (isDragging) {
      this._updateGizmos(state);
    }
  }

  _updateGizmos(state) {
    this.transformFinderResults = this.transformFinder.find(state);
  }
}
