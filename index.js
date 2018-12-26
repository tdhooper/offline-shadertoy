const fs = require('fs');
const main = require('./main.js');
const glslify = require('glslify');

// aura
// rhombille-triangle
// helix
// spiral-loop
// spiral-loop-2
// spiral-loop-pub
// spiral
// icosahedron-twist
// geodesic-tiling
// geodesic-tiling-free
// geodesic-twist
// helix-distance
// inverted-helix
// clifford-torus
// inverted-torus
// trefoil
// impossible-channel
// lines
// helix-wat
// peel
// rays-and-polygons

const frag = glslify('./projects/peel/shader.glsl');
const defaultState = JSON.parse(fs.readFileSync('./projects/peel/config.json', 'utf8'));
// const defaultState = null;

main(frag, defaultState);
