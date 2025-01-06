import createCube from 'primitive-cube';
import * as pexHelpers from '/lib/pex-helpers';

// 1 x 1 x 1 Box
const mesh = createCube();

const createDraw = function(uniforms) {

  const pass = pexHelpers.createPass({
    width: 1024,
    height: 1024,
    pixelFormat: ctx.PixelFormat.RGBA32F,
    depthTexture: true,
  });

  uniforms.uDepth = pexHelpers.passDepth(pass);
  uniforms.uSource = pexHelpers.passTex(pass);

  const drawPolygons = {
    pipeline: ctx.pipeline({
      vert: `
        precision mediump float;
        attribute vec3 position, normal;
        uniform mat4 model, view, projection;
        varying vec3 vnormal;
        void main() {
          vnormal = normal;
          gl_Position = projection * view * model * vec4(position, 1);
        }
      `,
      frag: `
        precision mediump float;
        varying vec3 vnormal;
        void main() {
          gl_FragColor = vec4(vnormal * .5 + .5, 1);
        }
      `,
      depthTest: true,
    }),
    attributes: {
      position: ctx.vertexBuffer(mesh.positions),
      normal: ctx.vertexBuffer(mesh.normals),
    },
    indices: ctx.indexBuffer(mesh.cells),
    uniforms: uniforms,
    pass: pass,
  };

  return function draw(state, drawShader) {
    if (
      pexHelpers.passTex(pass).width !== ctx.gl.drawingBufferWidth
      || pexHelpers.passTex(pass).height !== ctx.gl.drawingBufferHeight
    ) {
      pexHelpers.resizePass(pass, ctx.gl.drawingBufferWidth, ctx.gl.drawingBufferHeight);
    }

    ctx.apply(pexHelpers.evalCmd(drawPolygons, state));
    drawShader();
  };
};

export default createDraw;
