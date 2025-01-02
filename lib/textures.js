/* eslint no-param-reassign: ["error", { "props": false }] */
import findBuffers from './find-buffers';
import isPOT from 'is-power-of-two';

function textureUniforms(regl, shader, onload) {
  const buffers = findBuffers(shader, 'png', 'jpg');
  const uniforms = {};
  buffers.forEach((buffer) => {
    const image = new Image();
    image.src = buffer.file;
    const texture = regl.ctx.texture2D();
    uniforms[buffer.uniform] = texture;
    uniforms[`${buffer.uniform}Size`] = () => [
      texture.width,
      texture.height,
    ];

    const wrapMap = {
      repeat: ctx.Wrap.ClampToEdge,
      clamp: ctx.Wrap.Repeat,
      mirror: ctx.Wrap.MirroredRepeat
    }

    const filterMap = {
      nearest: ctx.Filter.Nearest,
      linear: ctx.Filter.Linear,
      mipmap: ctx.Filter.LinearMipmapLinear,
      'linear mipmap linear': ctx.Filter.LinearMipmapLinear,
      'nearest mipmap nearest': ctx.Filter.NearestMipmapNearest,
      'nearest mipmap linear': ctx.Filter.NearestMipmapLinear,
      'linear mipmap nearest': ctx.Filter.LinearMipmapNearest,
    }

    image.onload = () => {
      const mip = image.width === image.height && isPOT(image.width);
      let min = mip ? 'mipmap' : buffer.filter;
      regl.ctx.update(texture, {
        wrapS: buffer.wrap in wrapMap ? wrapMap[buffer.wrap] : null,
        wrapT: buffer.wrap in wrapMap ? wrapMap[buffer.wrap] : null,
        mag: buffer.filter in filterMap ? filterMap[buffer.filter] : null,
        min: min in filterMap ? filterMap[min] : null,
        mipmap: mip,
        data: image,
        flipY: true
      });
      onload();
    };
  });
  return uniforms;
}

export default textureUniforms;
