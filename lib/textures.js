/* eslint no-param-reassign: ["error", { "props": false }] */

function findTextures(shader) {
  // Reads comment after uniform, e.g.
  // uniform sampler2D iChannel0; // image.png, filter: linear, wrap: clamp
  const textures = [];
  const reUniform = /uniform\s+sampler2D\s+([^;]+).*\/\/(.*\.png.*)/g;
  const reFile = /([^\s]+\.png)/;
  const reFilter = /filter:\s*(\w+)/;
  const reWrap = /wrap:\s*(\w+)/;
  let match;
  do {
    match = reUniform.exec(shader);
    if (match) {
      textures.push({
        uniform: match[1],
        file: match[2].match(reFile)[1],
        filter: match[2].match(reFilter)[1],
        wrap: match[2].match(reWrap)[1],
      });
    }
  } while (match);

  const uniforms = {};

  textures.forEach((texInfo) => {
    const image = new Image();
    image.src = texInfo.file;
    const texture = global.regl.texture();
    uniforms[texInfo.uniform] = texture;
    image.onload = () => {
      texture({
        wrapS: texInfo.wrap,
        wrapT: texInfo.wrap,
        mag: texInfo.filter,
        min: texInfo.filter,
        data: image,
      });
    };
  });

  return uniforms;
}

module.exports = findTextures;
