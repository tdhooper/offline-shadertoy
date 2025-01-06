import { mat4, vec3, quat } from 'gl-matrix';
import gizmoShaderModifier from './gizmo-shader-modifier';
import * as pexHelpers from '/lib/pex-helpers';

export default class GizmoTransformFinder {

  evalGizmoPositions;
  evalGizmoResultsPass;
  gizmoCount;
  _draw;

  constructor(mapNode, uniforms, gizmoCount) {

    this.evalGizmoPositions = ctx.texture2D({
      width: 10,
      height: 1,
      pixelFormat: ctx.PixelFormat.RGBA32F,
    });
    
    this.evalGizmoResultsPass = pexHelpers.createPass({
      width: 10,
      height: gizmoCount,
      pixelFormat: ctx.PixelFormat.RGBA32F,
    });

    this.gizmoCount = gizmoCount;

    this._draw = this._createDraw(mapNode, uniforms);
  }

  _createDraw(mapNode, uniforms) {
    let shader = mapNode.shader;

    let result;
    result = gizmoShaderModifier.exposeArgumentsFromTransformMethod(shader, this.gizmoCount);
    result = gizmoShaderModifier.rewriteMainForTransformFinder(result.source, this.gizmoCount);
    shader = result.source;

    let gizmoUniforms = {
      evalGizmoPositions: this.evalGizmoPositions,
      evalGizmoPositionsResolution: (context, props) => {
        return [this.evalGizmoPositions.width, this.evalGizmoPositions.height];
      },
    };

    let drawGizmoTransformFinderCmd = {
      pass: this.evalGizmoResultsPass,
      pipeline: ctx.pipeline({
        vert: mapNode.vert,
        frag: shader,
      }),
      uniforms: gizmoUniforms,
    };

    return (state) => {
      state = Object.assign({}, state);
      mapNode.draw(state, drawGizmoTransformFinderCmd);
    };
  }

  _configure(positions) {

    // Update input buffer data
    let emptyData = Array(3 * 4).fill(0);
    let positionsData = positions.reduce((acc, p) => {return acc.concat([p[0], p[1], p[2], 0])}, []);
    let data = [].concat(emptyData, positionsData);
    ctx.update(this.evalGizmoPositions, {
      width: data.length / 4,
      height: 1,
      data: data,
    });

    // Set results framebuffer size
    if (pexHelpers.passTex(this.evalGizmoResultsPass).width != this.evalGizmoPositions.width) {
      pexHelpers.resizePass(this.evalGizmoResultsPass, this.evalGizmoPositions.width, this.gizmoCount);
    }
  }

  _vec3FromArray(array, index) {
    return new Float32Array(array.buffer, index * 16, 3);
  }

  _vec4FromArray(array, index) {
    return new Float32Array(array.buffer, index * 16, 4);
  }

  _samplePoints() {
    //*
    let points = [
      vec3.fromValues( 0,  0,  0),
    ];

    let size = 4;
    let count = 4;
    for (let x = 0; x < count; x++) {
      for (let y = 0; y < count; y++) {
        for (let z = 0; z < count; z++) {
          points.push(vec3.fromValues(
            (x / (count - 1)) * size - size / 2,
            (y / (count - 1)) * size - size / 2,
            (z / (count - 1)) * size - size / 2,
          ))
        }
      }
    }
    /*/
    let points = [
      vec3.fromValues( 0,  0,  0),
      vec3.fromValues( 1,  1,  1),
      vec3.fromValues( 1,  1, -1),
      vec3.fromValues( 1, -1,  1),
      vec3.fromValues( 1, -1, -1),
      vec3.fromValues(-1,  1,  1),
      vec3.fromValues(-1,  1, -1),
      vec3.fromValues(-1, -1,  1),
      vec3.fromValues(-1, -1, -1),
    ]
    //*/

    return points;
  }

  find(state) {
    const points = this._samplePoints()

    let sampleMatrices = points.map(point => {
      let world = mat4.create();
      mat4.fromRotationTranslationScale(
        world,
        quat.create(),
        point,
        vec3.fromValues(.01, .01, .01)
      );
      return world;
    });

    let samplePositions = sampleMatrices.reduce((acc, matrix) => {

      let positions = [
        vec3.fromValues(1, 0, 0),
        vec3.fromValues(0, 1, 0),
        vec3.fromValues(0, 0, 1),
        vec3.fromValues(0, 0, 0),
      ];

      positions.forEach(position => {
        vec3.transformMat4(position, position, matrix);
      });

      return acc.concat(positions);
    }, []);

    this._configure(samplePositions);
    this._draw(state);

    let cols = pexHelpers.passTex(this.evalGizmoResultsPass).width;
    let rows = pexHelpers.passTex(this.evalGizmoResultsPass).height;
    var bytes = new Float32Array(cols * rows * 4);

    pexHelpers.read({
      framebuffer: this.evalGizmoResultsPass.framebuffer,
      data: bytes,
    });

    let gizmosTransformArguments = [];

    for (let gizmoIndex = 0; gizmoIndex < this.gizmoCount; gizmoIndex++) {
      let t = this._vec3FromArray(bytes, cols * gizmoIndex + 0);
      let r = this._vec4FromArray(bytes, cols * gizmoIndex + 1);
      let s = this._vec3FromArray(bytes, cols * gizmoIndex + 2);
      gizmosTransformArguments.push({
        t, r, s,
      })
    }

    let gizmosParentTransformCandidates = [];

    for (let gizmoIndex = 0; gizmoIndex < this.gizmoCount; gizmoIndex++) {

      let parentTransformCandidates = [];
      let argumentsCount = 3;

      for (let i = 0; i < points.length; i++) {

        let x = this._vec3FromArray(bytes, cols * gizmoIndex + argumentsCount + i * 4 + 0);
        let y = this._vec3FromArray(bytes, cols * gizmoIndex + argumentsCount + i * 4 + 1);
        let z = this._vec3FromArray(bytes, cols * gizmoIndex + argumentsCount + i * 4 + 2);
        let o = this._vec3FromArray(bytes, cols * gizmoIndex + argumentsCount + i * 4 + 3);

        vec3.sub(x, x, o);
        vec3.sub(y, y, o);
        vec3.sub(z, z, o);

        let local = mat4.fromValues(
          x[0], x[1], x[2], 0,
          y[0], y[1], y[2], 0,
          z[0], z[1], z[2], 0,
          o[0], o[1], o[2], 1,
        );

        let inverseSampleMatrix = mat4.create();
        mat4.invert(inverseSampleMatrix, sampleMatrices[i]);
        mat4.multiply(local, local, inverseSampleMatrix);
        //mat4.invert(local, local);

        parentTransformCandidates.push(local);
      }

      gizmosParentTransformCandidates.push(parentTransformCandidates);
    }

    let gizmosParentTransform = [];

    for (let gizmoIndex = 0; gizmoIndex < this.gizmoCount; gizmoIndex++) {

      let lowestScore = Infinity;
      let lowestScoreIndex = 0;
      let difference = mat4.create();
      let parentTransformCandidates = gizmosParentTransformCandidates[gizmoIndex];

      parentTransformCandidates.forEach((transform, i) => {

        let score = parentTransformCandidates.reduce((totalScore, otherTransform) => {
          mat4.subtract(difference, transform, otherTransform);
          let differenceScore = difference.reduce((acc, value) => {return acc + Math.abs(value)}, 0);
          return totalScore + differenceScore;
        }, 0);

        if (score < lowestScore) {
          lowestScore = score;
          lowestScoreIndex = i;
        }
      });

      let parentTransform = parentTransformCandidates[lowestScoreIndex];
      gizmosParentTransform.push(parentTransform);
    }

    return {
      gizmosTransformArguments,
      gizmosParentTransform,
    };
  }
}
