import { mat4 } from 'gl-matrix';
import parseOBJ from 'parse-wavefront-obj';
import glslify from 'glslify';
import meshData from './skull.obj?raw';
import * as pexHelpers from '/lib/pex-helpers';

var mesh = parseOBJ(meshData);

const model = mat4.create();
// mat4.rotateY(model, model, .56);
// mat4.rotateZ(model, model, .01);
mat4.scale(model, model, [.55,.55,.55]);
mat4.translate(model, model, [.11,.6,0]);
mat4.rotateX(model, model, Math.PI / 2.);
mat4.rotateY(model, model, -.05);

const model2 = mat4.create();
mat4.scale(model2, model2, [-1,1,1]);
mat4.multiply(model2, model2, model);

function createDraw(uniforms) {
  const uu = Object.assign({}, uniforms);
  uu.model = pexHelpers.cmdProp('model');
  uu.albedo = pexHelpers.cmdProp('albedo');

  const buffer = pexHelpers.framebuffer({
    width: 1024,
    height: 1024,
    pixelFormat: ctx.PixelFormat.RGBA32F,
    depthTexture: true,
  });

  uniforms.uDepth = buffer.passCmd.framebuffer.depth.texture;
  uniforms.uSource = buffer.passCmd.framebuffer.color[0].texture;

  const drawPolygons = {
    // primitive: 'lines',
    pipeline: ctx.pipeline({

      vert: `
        precision mediump float;
        attribute vec3 position, normal;
        uniform mat4 model, view, projection;
        varying vec3 vnormal;
        void main() {
          vnormal = normalize((model * vec4(normal, 1)).xyz);
          gl_Position = projection * view * model * vec4(position, 1);
        }
      `,
      frag: glslify`
        #extension GL_OES_standard_derivatives : enable
        precision mediump float;
        uniform bool guiModel;
        uniform vec3 albedo;
        varying vec3 vnormal;
        void main() {
          vec3 color = vnormal * .5 + .5;
          vec3 lig = vec3(0,1.5,.5);
          lig = normalize(vec3(1,1,0));
          if ( ! guiModel) {
            color = albedo;
            color *= pow(clamp(dot(lig, vnormal) * .5 + .5, 0., 1.), 1./2.2);
          }
          gl_FragColor = vec4(color, 1);
        }
      `,
    }),
    attributes: {
      position: ctx.vertexBuffer(mesh.positions),
      normal: ctx.vertexBuffer(mesh.vertexNormals),
    },
    indices: ctx.indexBuffer(mesh.cells),
    uniforms: uu,
    pass: buffer.clearPassCmd,
  };

  return function draw(state, drawShader) {

    if (
      buffer.size().width !== ctx.gl.drawingBufferWidth
      || buffer.size().height !== ctx.gl.drawingBufferHeight
    ) {
      buffer.resize(ctx.gl.drawingBufferWidth, ctx.gl.drawingBufferHeight);
    }

    ctx.apply(pexHelpers.evalCmd(drawPolygons, Object.assign({model: model, albedo: [1,1,1]}, state)));
    drawShader();
  };
}

export default createDraw;
