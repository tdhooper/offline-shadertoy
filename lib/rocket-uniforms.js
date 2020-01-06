function findUniforms(shader) {
  // Reads comment after uniform, e.g.
  // uniform float foo; // rocket
  const names = [];
  const reUniform = /uniform\s+float\s+([^;]+).*\/\/(.*)/g;
  let uniformMatch;
  let name;
  do {
    uniformMatch = reUniform.exec(shader);
    name = uniformMatch && uniformMatch[1];
    if (name) {
      names.push(name);
    }
  } while (uniformMatch);
  return names;
}

const create = (shaders, uniforms) => {
  const names = shaders.map(findUniforms).flat();
  const values = {};
  names.forEach((name) => {
    values[name] = 0;
    uniforms[name] = () => values[name];
  });
  return values;
};

module.exports = create;
