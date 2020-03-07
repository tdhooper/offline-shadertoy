const fs = require('fs');
const { mat4 } = require('gl-matrix');
const parseOBJ = require('parse-wavefront-obj');
const glslify = require('glslify');


const meshData = fs.readFileSync('projects/skull/skull.obj');
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

function init(drawRaymarch, renderNodes, uniforms) {
  const uu = Object.assign({}, uniforms);
  uu.model = regl.prop('model');
  uu.albedo = regl.prop('albedo');

  const buffer = regl.framebuffer({
    width: 1024,
    height: 1024,
    depthTexture: true,
  });

  const drawPolygons = global.regl({
    // primitive: 'lines',
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
      uniform bool guiSplit;
      uniform vec3 albedo;
      varying vec3 vnormal;
      void main() {
        vec3 color = vnormal * .5 + .5;
        vec3 lig = vec3(0,1.5,.5);
        lig = normalize(vec3(1,1,0));
        // if ( ! guiSplit) {
          color = albedo * pow(clamp(dot(lig, vnormal) * .5 + .5, 0., 1.), 1./2.2);
        // }
        gl_FragColor = vec4(color, 1);
      }
    `,
    attributes: {
      position: mesh.positions,
      normal: mesh.vertexNormals,
    },
    elements: mesh.cells,
    uniforms: uu,
    framebuffer: buffer,
  });

  const setup = global.regl({
    uniforms: {
      uDepth: buffer.depthStencil,
      uSource: buffer,
    },
  });

  return function draw(state, context) {
    global.regl.clear({
      color: [0, 0, 0, 1],
      depth: 1,
      framebuffer: buffer,
    });

    if (
      buffer.width !== context.viewportWidth
      || buffer.height !== context.viewportHeight
    ) {
      buffer.resize(context.viewportWidth, context.viewportHeight);
    }

    drawPolygons(Object.assign({model: model, albedo: [1,1,1]}, state));
    drawPolygons(Object.assign({model: model2, albedo: [1,1,1]}, state));

    setup(state, (context) => {
      drawRaymarch(state, () => {
        renderNodes.forEach((node) => {
          node.draw(state);
        });
      });
    });
  };
}

module.exports = init;
// module.exports = function(){};
