/* eslint no-param-reassign: ["error", { "props": false }] */
import findBuffers from './find-buffers';
import isPOT from 'is-power-of-two';
import * as pexHelpers from './pex-helpers';

function textureUniforms(ctx, shader, onload) {
  const buffers = findBuffers(shader, 'png', 'jpg');
  const uniforms = {};
  buffers.forEach((buffer) => {
    const image = new Image();
    image.src = buffer.file;
    const texture = ctx.texture2D({});
    uniforms[buffer.uniform] = texture;
    uniforms[`${buffer.uniform}Size`] = () => [
      texture.width,
      texture.height,
    ];

    image.onload = () => {
//      const mip = image.width === image.height && isPOT(image.width);
//      let min = mip ? 'mipmap' : buffer.filter;
      let filter = pexHelpers.filterConst(buffer.filter);
      //let wrap = pexHelpers.wrapConst(dep.wrap);
      ctx.update(texture, {
        // wrap: wrap,
        mag: filter,
        min: filter,
//        mipmap: mip,
        data: image,
        flipY: true
      });
      onload();
    };
  });
  return uniforms;
}

export default textureUniforms;
