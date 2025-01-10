import gizmoShaderModifier from './gizmo-shader-modifier';
import * as pexHelpers from '/lib/pex-helpers';

export default class GizmoRendererHooks {

  shaderFiles = [];
  gizmoCount;
  transformFinder;

  preprocessShaders(shaders) {
    shaders.forEach((shader) => {
      if (this._preprocessShader(shader)) {
        this.shaderFiles.push(shader.file);
      }
    });

    if ( ! this.gizmoCount) {
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

  preprocessUniforms(uniforms) {
    if ( ! this.gizmoCount) {
      return;
    }

    uniforms['gizmoAdjust[]'] = pexHelpers.cmdProp('gizmoAdjust');
    uniforms['gizmoAdjustment[]{}'] = pexHelpers.cmdProp('gizmoAdjustment');
  }

  preprocessRenderNodes(renderNodes) {
    if ( ! this.gizmoCount) {
      return;
    }

    this.mapNode = this._findMapNode(renderNodes);

    if ( ! this.mapNode) {
      console.log('Could not find GIZMO_MAP function');
    }
  }

  _findMapNode(renderNodes) {
    for (let i = renderNodes.length - 1; i >= 0; i--) {
      let node = renderNodes[i];
      if (node.shader.indexOf('GIZMO_MAP(') != -1) {
        return node;
      }
    }
  }
}
