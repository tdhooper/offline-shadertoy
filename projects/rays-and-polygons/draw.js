import createCube from 'primitive-cube';
import { mat4 } from 'gl-matrix';


// 1 x 1 x 1 Box
const mesh = createCube();

const createDraw = function(uniforms, setupProjectionView) {

  const drawPolygons = window.regl({
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
  });

  return function draw(state, drawShader) {
    setupProjectionView(state, (context) => {
      drawPolygons(state);
    });
    drawShader();
  };
};

export default createDraw;
