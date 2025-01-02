import { mat4, vec3, quat } from 'gl-matrix';
import gizmoShaderModifier from './gizmo-shader-modifier';
import quadVertShader from '../../quad.vert';

export default class GizmoTransformFinder {

  evalGizmoPositions;
  evalGizmoResults;
  _draw;

  constructor(mapNode, uniforms) {

    this.evalGizmoPositions = regl.ctx.texture2D({
      width: 10,
      height: 1,
      pixelFormat: regl.ctx.PixelFormat.RGBA32F,
    });
    
    this.evalGizmoResults = regl.framebuffer({
      width: 10,
      height: 1,
      pixelFormat: regl.ctx.PixelFormat.RGBA32F,
    });

    this._draw = this._createDraw(mapNode, uniforms);
  }

  _createDraw(mapNode, uniforms) {
    let shader = mapNode.shader;

    shader = gizmoShaderModifier.exposeArgumentsFromTransformMethod(shader);
    shader = gizmoShaderModifier.rewriteMainForTransformFinder(shader);

    let gizmoUniforms = Object.assign({
      evalGizmoPositions: this.evalGizmoPositions,
      evalGizmoPositionsResolution: (context, props) => {
        return [this.evalGizmoPositions.width, this.evalGizmoPositions.height];
      },
    }, uniforms);

    let drawGizmoTransformFinder = regl({
      vert: quadVertShader,
      frag: shader,
      uniforms: gizmoUniforms,
      framebuffer: this.evalGizmoResults,
    });

    return (state) => {
      state = Object.assign({}, state);
      state.gizmoAdjust = false;
      mapNode.draw(state, () => {
        drawGizmoTransformFinder(state);
      });
    };
  }

  _configure(positions) {

    // Update input buffer data
    let emptyData = Array(3 * 4).fill(0);
    let positionsData = positions.reduce((acc, p) => {return acc.concat([p[0], p[1], p[2], 0])}, []);
    let data = [].concat(emptyData, positionsData);
    regl.ctx.update(this.evalGizmoPositions, {
      width: data.length / 4,
      height: 1,
      data: data,
    });

    // Set results framebuffer size
    if (this.evalGizmoResults.tex.width != this.evalGizmoPositions.width) {
      this.evalGizmoResults.resize(this.evalGizmoPositions.width, this.evalGizmoPositions.height);
    }

    // Clear results buffer
    regl.clear({
      color: [0,0,0,1],
      depth: 1,
      stencil: 0,
      framebuffer: this.evalGizmoResults,
    });
  }

  _vec3FromArray(array, index) {
    return new Float32Array(array.buffer, index * 16, 3);
  }

  _vec4FromArray(array, index) {
    return new Float32Array(array.buffer, index * 16, 4);
  }

  _findOriginTransformsFromSourcePoints(state, points) {

    let worldMatrices = points.map(point => {
      let world = mat4.create();
      mat4.fromRotationTranslationScale(
        world,
        quat.create(),
        point,
        vec3.fromValues(.01, .01, .01)
      );
      return world;
    });

    let positions = worldMatrices.reduce((acc, world) => {

      let positions = [
        vec3.fromValues(1, 0, 0),
        vec3.fromValues(0, 1, 0),
        vec3.fromValues(0, 0, 1),
        vec3.fromValues(0, 0, 0),
      ];

      positions.forEach(position => {
        vec3.transformMat4(position, position, world);
      });

      return acc.concat(positions);
    }, []);

    this._configure(positions);
    this._draw(state);

    let cols = this.evalGizmoResults.tex.width;
    let rows = this.evalGizmoResults.tex.height;
    var bytes = new Float32Array(cols * rows * 4);

    regl.read({
      framebuffer: this.evalGizmoResults,
      data: bytes,
    });

    let t = this._vec3FromArray(bytes, 0);
    let r = this._vec4FromArray(bytes, 1);
    let s = this._vec3FromArray(bytes, 2);

    let parentTransforms = [];
    let argumentsCount = 3;

    for (let i = 0; i < points.length; i++) {

      let x = this._vec3FromArray(bytes, argumentsCount + i * 4 + 0);
      let y = this._vec3FromArray(bytes, argumentsCount + i * 4 + 1);
      let z = this._vec3FromArray(bytes, argumentsCount + i * 4 + 2);
      let o = this._vec3FromArray(bytes, argumentsCount + i * 4 + 3);

      vec3.sub(x, x, o);
      vec3.sub(y, y, o);
      vec3.sub(z, z, o);

      let local = mat4.fromValues(
        x[0], x[1], x[2], 0,
        y[0], y[1], y[2], 0,
        z[0], z[1], z[2], 0,
        o[0], o[1], o[2], 1,
      );

      let inverseWorld = mat4.create();
      mat4.invert(inverseWorld, worldMatrices[i]);
      mat4.multiply(local, local, inverseWorld);
      //mat4.invert(local, local);

      parentTransforms.push(local);
    }

    return {
      parentTransforms: parentTransforms,
      transformArguments: {t, r, s},
    };
  }

  find(state) {
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

    let results = this._findOriginTransformsFromSourcePoints(state, points);

    let lowestScore = Infinity;
    let lowestScoreIndex = 0;
    let difference = mat4.create();

    results.parentTransforms.forEach((transform, i) => {

      let score = results.parentTransforms.reduce((totalScore, otherTransform) => {
        mat4.subtract(difference, transform, otherTransform);
        let differenceScore = difference.reduce((acc, value) => {return acc + Math.abs(value)}, 0);
        return totalScore + differenceScore;
      }, 0);

      if (score < lowestScore) {
        lowestScore = score;
        lowestScoreIndex = i;
      }
    });

    return {
      parentTransform: results.parentTransforms[lowestScoreIndex],
      transformArguments: results.transformArguments,
    };
  }
}
