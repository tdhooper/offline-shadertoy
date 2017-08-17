var Regl = require('regl');
var glslify = require('glslify');
var mouseChange = require('mouse-change');
var debounce = require('debounce');
var Timer = require('./lib/timer');
var stateStore = require('./lib/state-store');
var FileSaver = require('file-saver');
var pad = require('pad-number');

var pixelRatio = window.devicePixelRatio;
pixelRatio = .5;

var canvas = document.createElement('canvas');
document.body.appendChild(canvas);

var controls = document.createElement('div');
controls.classList.add('controls');
document.body.appendChild(controls);

var scrubber = document.createElement('input');
scrubber.classList.add('scrubber');
scrubber.setAttribute('type', 'range');
scrubber.min = 0;
scrubber.max = 1000 * 8; // milliseconds
scrubber.step = 10;
controls.appendChild(scrubber);


function setDimensions() {
    canvas.width = canvas.offsetWidth * pixelRatio;
    canvas.height = canvas.offsetHeight * pixelRatio;
}
setDimensions();

var regl = Regl({
    canvas: canvas,
    pixelRatio: pixelRatio
});

var vert = glslify('./quad.vert');
var frag = glslify('./projects/dod-fract-optimise/shader.glsl');

var texture = regl.texture();

var image = new Image();
image.src = '/images/noise.png';
image.onload = function() {
    texture({
        data: image,
        mag: 'linear',
        min: 'mipmap',
        wrapS: 'repeat',
        wrapT: 'repeat'
    });
    render();
};

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
        iResolution: function(context, props) {
            var resolution = [context.viewportWidth, context.viewportHeight];
            return props.resolution || resolution;
        },
        iOffset: function(context, props) {
            return props.offset || [0, 0];
        },
        iGlobalTime: regl.prop('time'),
        iMouse: function(context, props) {
            var mouse = props.mouse.map(function(value) {
                return value * context.pixelRatio;
            });
            mouse[1] = context.viewportHeight - mouse[1];
            //console.log(mouse[0] / context.viewportWidth);
            //console.log(mouse[1] / context.viewportHeight)
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
        window.timer = timer; 
    }
    mouse = state.mouse || mouse;
}

function render(offset, resolution) {
    var time = timer.elapsed();
    scrubber.value = time;
    saveState();
    drawTriangle({
        time: time / 1000,
        mouse: mouse,
        offset: offset,
        resolution: resolution
    });
}

function play() {
    timer.play();
    tick = regl.frame(function() {
        render();
    });
}

function pause() {
    if (timer.running) {
        timer.pause();
        tick && tick.cancel();
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

function stepTo(time) {
    timer.set(time);
    render();
}

window.play = play;
window.pause = pause;
window.stop = stop;
window.toggle = toggle;
window.stepTo = stepTo;

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

function scrub(evt) {
    pause();
    stepTo(parseFloat(this.value));
};

scrubber.addEventListener('change', scrub);
scrubber.addEventListener('mousedown', function() {
    this.addEventListener('mousemove', scrub);
});
scrubber.addEventListener('mouseup', function() {
    this.removeEventListener('mousemove', scrub);
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
            setDimensions();
            regl._refresh();
            render();
        },
        50
    )
);


function chunkDimension(size, maxSize) {
    var count = Math.ceil(size / maxSize);
    var chunkSize = Math.ceil(size / count);
    var chunks = [];
    var offset = 0;
    while (size > 0) {
        size -= chunkSize;
        if (size > 0) {
            chunks.push({
                offset: offset,
                size: chunkSize
            });
            offset += chunkSize;
        } else {
            chunks.push({
                offset: offset,
                size: chunkSize + size
            });
        }
    }
    return chunks;
}

function chunkDimensions(width, height, maxSize) {
    var widthChunks = chunkDimension(width, maxCanvasSize);
    var heightChunks = chunkDimension(height, maxCanvasSize);

    var chunks = [];
    heightChunks.reverse();

    heightChunks.forEach(function(heightChunk) {
        widthChunks.forEach(function(widthChunk) {
            chunks.push({
                offset: [widthChunk.offset, heightChunk.offset],
                size: [widthChunk.size, heightChunk.size]
            });
        });
    });

    return chunks;
}

var maxCanvasSize = 4000;

function save(width, height) {
    width = width || 500;
    height = height || 500;

    var resolution = [width, height];
    var chunks = chunkDimensions(width, height, maxCanvasSize);

    var saveChunk = function(index) {
        var chunk = chunks[index];

        canvas.width = chunk.size[0];
        canvas.height = chunk.size[1];
        canvas.style.width = chunk.size[0] + 'px';
        canvas.style.height = chunk.size[1] + 'px';

        regl._refresh();
        render(chunk.offset, resolution);

        canvas.toBlob(function(blob) {
            var digits = chunks.length.toString().length;
            var filename = 'image-' + pad(index + 1, digits) + '.png';
            FileSaver.saveAs(blob, filename);
            if (index < chunks.length - 1) {
                saveChunk(index + 1);
            } else {
                canvas.style.width = null;
                canvas.style.height = null;
                setDimensions();
                regl._refresh();
                render();
            }
        });
    };

    saveChunk(0);
}

window.save = save;
