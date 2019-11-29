
function getMatch(string, regex, index, fallback) {
  const match = string.match(regex);
  if (match) {
    return match[index];
  }
  return fallback;
}

function findBuffers(shader, ...filetypes) {
  // Reads comment after uniform, e.g.
  // uniform sampler2D iChannel0; // buffer-a.glsl, filter: linear, wrap: clamp
  const dependencies = [];
  const reUniform = /uniform\s+sampler2D\s+([^;]+).*\/\/(.*)/g;
  const reFile = new RegExp(`([^\\s]+)\\.(${filetypes.join('|')})`);
  const reFilter = /filter:\s*(\w+)/;
  const reWrap = /wrap:\s*(\w+)/;
  let uniformMatch;
  let fileMatch;
  do {
    uniformMatch = reUniform.exec(shader);
    fileMatch = uniformMatch && uniformMatch[2].match(reFile);
    if (fileMatch) {
      dependencies.push({
        uniform: uniformMatch[1],
        name: fileMatch[1],
        file: fileMatch[0],
        filter: getMatch(uniformMatch[2], reFilter, 1, 'linear'),
        wrap: getMatch(uniformMatch[2], reWrap, 1, 'clamp'),
      });
    }
  } while (uniformMatch);
  return dependencies;
}

module.exports = findBuffers;
