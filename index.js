var regl = require('regl')();
var glslify = require('glslify');
var mouseChange = require('mouse-change');
var debounce = require('debounce');
var Timer = require('./lib/timer')
var stateStore = require('./lib/state-store');

var vert = glslify('./quad.vert');
var frag = glslify('./quad.frag');

var texture = regl.texture();

var image = new Image()
image.src = '/images/noise.png'
image.onload = function() {
    texture({
        data: image,
        mag: 'linear',
        min: 'mipmap',
        wrapS: 'repeat',
        wrapT: 'repeat'
    });
    render();
}

const drawTriangle = regl({
    frag: frag,
    vert: vert,
    attributes: {
        position: [
            [-2, 0],
            [0, -2],
            [2,  2]
        ]
    },
    uniforms: {
        iResolution: function(context) {
            return [context.viewportWidth, context.viewportHeight];
        },
        iGlobalTime: regl.prop('time'),
        iMouse: function(context, props) {
            var mouse = props.mouse.map(function(value) {
                return value * context.pixelRatio;
            });
            mouse[1] = context.viewportHeight - mouse[1];
            console.log(mouse[0] / context.viewportWidth);
            console.log(mouse[1] / context.viewportHeight)
            return mouse;
        },
        iChannel0: texture
    },

    // This tells regl the number of vertices to draw in this command
    count: 3
});


var timer = new Timer();
var mouse = [0,0,0,0];

function saveState() {
    stateStore.save('state', {
        timer: timer.serialize(),
        mouse: mouse
    });
}

function restoreState() {
    var state = stateStore.restore('state');
    if ( ! state) {
        return;
    }
    if (state.timer) {
        timer = Timer.fromObject(state.timer);    
    }
    mouse = state.mouse || mouse;
}

function render() {
    var time = timer.elapsed();
    saveState();
    drawTriangle({
        time: time / 1000,
        mouse: mouse
    });
}

function play() {
    timer.play();
    tick = regl.frame(render);
}

function pause() {
    if (timer.running) {
        timer.pause();
        tick && tick.cancel()
        saveState();
    }
}

function stop() {
    pause();
    timer.reset();
    render();
}

function toggle() {
    if (timer.running) {
        pause();
    } else {
        play();
    }
}

window.play = play;
window.pause = pause;
window.stop = stop;
window.toggle = toggle;

var canvas = regl._gl.canvas;
mouseChange(canvas, function(buttons, x, y, mods) {
    var lmbPressed = buttons == 1;
    if (lmbPressed) {
        mouse = [x, y, 0, 0];
        if ( ! timer.running) {
            render();
        }
    }
});

restoreState();

if (timer.running) {
    play();
} else {
    render();    
}

window.addEventListener(
    'resize',
    debounce(
        function() {
            regl._refresh();
            render();
        },
        50
    )
);
