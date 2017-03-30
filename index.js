var Geometry = require('gl-geometry');
var glShader = require('gl-shader');
var glslify = require('glslify');
var loop = require('raf-loop');
var mouseChange = require('mouse-change');

var canvas = document.createElement('canvas');
canvas.style.width = '100%';
canvas.style.height = '100%';
canvas.style.position = 'fixed';
canvas.style.top = '0';
canvas.style.left = '0';
document.body.appendChild(canvas);

var gl = canvas.getContext('webgl');

function setDimensions() {    
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
};

setDimensions();

var quad = Geometry(gl)
    .attr('aPosition', [
        -1, -1, 0,
         1, -1, 0,
         1,  1, 0,
        -1, -1, 0,
         1,  1, 0,
        -1,  1, 0
    ]);

var vert = glslify('./quad.vert');
var frag = glslify('./quad.frag')
var program = glShader(gl, vert, frag);

gl.clearColor(1,0,1,1);

var startPlaying = false;
var time = 0;
var mouse = [0,0,0,0];

var engine = loop(function(dt) {
    time += dt;
    render();
});

function render() {
    saveState();
    gl.clear(gl.COLOR_BUFFER_BIT);
    program.bind();
    program.uniforms.iResolution = [canvas.width, canvas.height];
    program.uniforms.iGlobalTime = time/1000.0;
    program.uniforms.iMouse = mouse;
    quad.bind(program);
    quad.draw();
}

function saveState() {
    var state = {
        startPlaying: engine.running,
        mouse: mouse,
        time: time
    };
    localStorage.setItem('uniformsState', JSON.stringify(state))
}

function restoreState() {
    var stateStr = localStorage.getItem('uniformsState');
    var state = stateStr && JSON.parse(stateStr);
    if (state && state.hasOwnProperty('mouse')) {
        mouse = state.mouse;
    }
    if (state && state.hasOwnProperty('time')) {
        time = state.time;
    }
    if (state && state.hasOwnProperty('startPlaying')) {
        startPlaying = state.startPlaying;
    }
}

window.play = function() { engine.start(); };
window.pause = function() { engine.stop(); };

window.onresize = function() {
    setDimensions();
    if ( ! engine.running) {
        render();
    }
};

mouseChange(canvas, function(buttons, x, y, mods) {
    var lmbPressed = buttons == 1;
    if (lmbPressed) {
        mouse = [x, canvas.height - y, 0, 0];
        if ( ! engine.running) {
            render();
        }
    }
});

restoreState();
render();

if (startPlaying) {
    engine.start();
}
