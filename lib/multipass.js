/* eslint no-param-reassign: ["error", { "props": false }] */

function findBuffers(shader) {
  // Reads comment after uniform, e.g.
  // uniform sampler2D iChannel0; // buffer-a.glsl, filter: linear, wrap: clamp
  const dependencies = [];
  const reUniform = /uniform\s+sampler2D\s+([^;]+).*\/\/(.*)/g;
  const reFile = /([^\s]+)\.glsl/;
  const reFilter = /filter:\s*(\w+)/;
  const reWrap = /wrap:\s*(\w+)/;
  let match;
  do {
    match = reUniform.exec(shader);
    if (match) {
      dependencies.push({
        uniform: match[1],
        name: match[2].match(reFile)[1],
        filter: match[2].match(reFilter)[1],
        wrap: match[2].match(reWrap)[1],
      });
    }
  } while (match);
  return dependencies;
}

function nodesFromShaders(shaders) {
  const nodes = {};
  Object.entries(shaders).forEach(([name, shader]) => {
    nodes[name] = {
      name,
      shader,
      dependencies: findBuffers(shader),
    };
  });
  Object.entries(nodes).forEach(([name, node]) => {
    node.dependencies.forEach((dep) => {
      dep.node = nodes[dep.name];
    });
  });
  return nodes;
}

// https://www.electricmonk.nl/docs/dependency_resolving_algorithm/dependency_resolving_algorithm.html
function resolveDependency(node, resolved, unresolved) {
  unresolved.push(node);
  node.dependencies.forEach((dep) => {
    const depNode = dep.node;
    if (resolved.indexOf(depNode) === -1) {
      if (unresolved.indexOf(depNode) !== -1) {
        return;
        // throw new Error(`Circular reference detected: ${node.name} -> ${depNode.name}`);
      }
      resolveDependency(depNode, resolved, unresolved);
    }
  });
  resolved.push(node);
  unresolved.push(node);
}

function buildRenderNodes(shaders) {
  const nodes = nodesFromShaders(shaders);
  const renderOrder = [];
  resolveDependency(nodes.main, renderOrder, []);
  return renderOrder;
}

module.exports = buildRenderNodes;
