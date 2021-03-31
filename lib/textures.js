/* eslint no-param-reassign: ["error", { "props": false }] */
const findBuffers = require('./find-buffers');
const isPOT = require('is-power-of-two');

function textureUniforms(regl, shader, onload) {
  const buffers = findBuffers(shader, 'png', 'jpg');
  const uniforms = {};
  buffers.forEach((buffer) => {
    const image = new Image();
    image.src = buffer.file;
    const texture = regl.texture();
    uniforms[buffer.uniform] = texture;
    uniforms[`${buffer.uniform}Size`] = () => [
      texture.width,
      texture.height,
    ];

    image.onload = () => {
      const mip = image.width === image.height && isPOT(image.width);
      texture({
        wrapS: buffer.wrap,
        wrapT: buffer.wrap,
        mag: buffer.filter,
        min: mip ? 'mipmap' : buffer.filter,
        mipmap: mip,
        data: image,
        flipY: true
      });
      onload();
    };
  });
  return uniforms;
}

module.exports = textureUniforms;
