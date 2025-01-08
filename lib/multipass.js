/* eslint no-param-reassign: ["error", { "props": false }] */
import findBuffers from './find-buffers';
import quadVertShader from '/quad.vert?raw';
import quadVertShaderWebGL2 from '/quad-webgl2.vert?raw';

function nodesFromShaders(shaders) {
  const reConfig = /\/\/\s*framebuffer\s(.*)/;
  const reSize = /size:\s*(\d+)[x\s](\d+)/;
  const reTile = /tile:\s*(\d+)/;
  const reFirst = /firstpassonly/;
  const reDrawCount = /drawcount:\s*(\d+)/;

  const nodes = {};
  Object.entries(shaders).forEach(([name, shader]) => {
    let webgl2 = shader.glsl.indexOf('#version 300 es') !== -1;
    const vert = webgl2 ? quadVertShaderWebGL2 : quadVertShader;
    const node = {
      name,
      shader: shader.glsl,
      vert: vert,
      dependencies: findBuffers(shader.glsl, 'glsl', 'frag'),
      drawCount: 1,
    };
    const configMatch = shader.glsl.match(reConfig);
    if (configMatch) {
      const sizeMatch = configMatch[1].match(reSize);
      if (sizeMatch) {
        node.size = sizeMatch.slice(1, 3).map(n => parseInt(n, 10));
      }
      const tileMatch = configMatch[1].match(reTile);
      if (tileMatch) {
        node.tile = parseInt(tileMatch[1], 10);
      }
      const drawCountMatch = configMatch[1].match(reDrawCount);
      if (drawCountMatch) {
        node.drawCount = parseInt(drawCountMatch[1], 10);
      }
      node.firstPassOnly = Boolean(configMatch[1].match(reFirst));
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
  renderOrder[renderOrder.length - 1].final = true;
  return renderOrder;
}

export default buildRenderNodes;
