/* eslint no-param-reassign: ["error", { "props": false }] */
const findBuffers = require('./find-buffers');

function nodesFromShaders(shaders) {
  const reSize = /framebuffer size:\s+(\d+)x(\d+)/;
  const nodes = {};
  Object.entries(shaders).forEach(([name, shader]) => {
    const node = {
      name,
      shader,
      dependencies: findBuffers(shader, 'glsl'),
    };
    const match = shader.match(reSize);
    if (match) {
      node.size = match.slice(1, 3).map(n => parseInt(n, 10));
    }
    nodes[name] = node;
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
