/* eslint no-param-reassign: ["error", { "props": false }] */
const findBuffers = require('./find-buffers');

function textureUniforms(regl, shader) {
  const buffers = findBuffers(shader, 'png');
  const uniforms = {};
  buffers.forEach((buffer) => {
    const image = new Image();
    image.src = buffer.file;
    const texture = regl.texture();
    uniforms[buffer.uniform] = texture;
    image.onload = () => {
      texture({
        wrapS: buffer.wrap,
        wrapT: buffer.wrap,
        mag: buffer.filter,
        min: buffer.filter,
        data: image,
      });
    };
  });
  return uniforms;
}

module.exports = textureUniforms;
