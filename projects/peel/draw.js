const fs = require('fs');
const { mat4 } = require('gl-matrix');
const parseOBJ = require('parse-wavefront-obj');
// const wireframe = require('gl-wireframe');
const solidWireframe = require('glsl-solid-wireframe');
const glslify = require('glslify');


const meshData = fs.readFileSync('projects/peel/model4.obj');
var mesh = parseOBJ(meshData);
// mesh.cells = wireframe(mesh.cells);
mesh = solidWireframe(mesh, {
  attributes: {
    vertexNormals: mesh.vertexNormals,
  },
});

const model = mat4.create();
mat4.rotateX(model, model, -.38);
mat4.rotateY(model, model, .56);
mat4.rotateZ(model, model, .01);
mat4.translate(model, model, [.225,-.5,.15]);
mat4.scale(model, model, [50, 50, 50]);

const drawPolygons = global.regl({
  // primitive: 'lines',
  vert: `
    precision mediump float;
    attribute vec3 position, normal;
    uniform mat4 model, view, projection;
    attribute vec2 barycentric;
    varying vec2 b;
    varying vec3 vnormal;
    void main() {
      b = barycentric;
      vnormal = normalize((model * vec4(normal, 1)).xyz);
      gl_Position = projection * view * model * vec4(position, 1);
    }
  `,
  frag: glslify`
    #extension GL_OES_standard_derivatives : enable
    precision mediump float;
    varying vec3 vnormal;
    #pragma glslify: bary_wire_scaled = require(glsl-solid-wireframe/barycentric/scaled)
    varying vec2 b;
    void main() {
      vec3 color = vnormal * .5 + .5;
      color = vec3(1);
      color *= dot(vec3(1,-1,0), vnormal) * .5 + .5;
      float line = bary_wire_scaled(b, .2);
      // color *= line;
      // color = vec3(0,1,0);
      gl_FragColor = vec4(color, 1);
    }
  `,
  attributes: {
    position: mesh.positions,
    barycentric: mesh.barycentric,
    normal: mesh.attributes.vertexNormals,
  },
  elements: mesh.cells,
  uniforms: {
    // model: function(context, props) {
    //   return mat4.fromTranslation([], props.camera.position);
    // }
    model: model,
  },
});

module.exports = drawPolygons;
// module.exports = function(){};
