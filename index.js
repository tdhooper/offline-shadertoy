var Regl = require('regl');
var glslify = require('glslify');
var mouseChange = require('mouse-change');
var debounce = require('debounce');
var Timer = require('./lib/timer');
var stateStore = require('./lib/state-store');
var FileSaver = require('file-saver');
var pad = require('pad-number');
var fs = require('fs');
var GUI = require('./lib/gui');
var WebCaptureClient = require('web-frames-capture');
var pixelRatio = window.devicePixelRatio;
var createCamera = require('./lib/free-fly-camera');
var pressed = require('key-pressed');

pixelRatio = 1.;

var canvas = document.createElement('canvas');
document.body.appendChild(canvas);

var controls = document.createElement('div');
controls.classList.add('controls');
document.body.appendChild(controls);

var scrubber = document.createElement('input');
scrubber.classList.add('scrubber');
scrubber.setAttribute('type', 'range');
scrubber.min = 0;
scrubber.max = 2000; // milliseconds
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
var frag = glslify('./projects/spiral-loop/shader.glsl');

var guiConf = JSON.parse(fs.readFileSync('./projects/spiral-loop/gui.json', 'utf8'));
var stateConf = JSON.parse(fs.readFileSync('./projects/spiral-loop/state.json', 'utf8'));

var gui = new GUI(guiConf);

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

const uniforms = {
    iResolution: function(context, props) {
        var resolution = [context.viewportWidth, context.viewportHeight];
        return props.resolution || resolution;
    },
    iOffset: function(context, props) {
        return props.offset || [0, 0];
    },
    cameraMatrix: regl.prop('cameraMatrix'),
    cameraPosition: regl.prop('cameraPosition'),
    iGlobalTime: regl.prop('time'),
    iTime: regl.prop('time'),
    iMouse: function(context, props) {
        var mouse = props.mouse.map(function(value) {
            return value * context.pixelRatio;
        });
        mouse[1] = context.viewportHeight - mouse[1];
        // console.log(mouse[0] / context.viewportWidth);
        // console.log(mouse[1] / context.viewportHeight)
        return mouse;
    },
    iChannel0: texture
};

addGuiUniforms = function(prefix, state) {
    Object.keys(state).forEach(function(key) {
        var name = prefix + key[0].toUpperCase() + key.slice(1);
        if (state[key] instanceof Object) {
            addGuiUniforms(name, state[key]);
            return;
        }
        uniforms[name] = function(context, props) {
            return props.gui[key];
        };
    });
};

addGuiUniforms('gui', gui.state);

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
    uniforms: uniforms,

    // This tells regl the number of vertices to draw in this command
    count: 3
});

var timer = new Timer();
var mouse = [0,0,0,0];

function getState() {
    return {
        timer: timer.serialize(),
        mouse: mouse,
        gui: gui.saveState(),
        cameraMatrix: camera.view()
    };
}

function saveState() {
    stateStore.save('state', getState());
}

function restoreState(state) {
    if ( ! state) {
        return;
    }
    if (state.timer) {
        timer = Timer.fromObject(state.timer);
        window.timer = timer; 
    }
    mouse = state.mouse || mouse;
    gui.loadState(state.gui);
    if (state.cameraMatrix) {
        camera = createCamera({
            view: state.cameraMatrix
        });
    }
}

var frameCount = 0;
var fpsTimeout;

var camera = createCamera({
    position: [0,0,-1]
});
var lastMouse;
var startMouse = [0,0];
var lastConfigJson;

var lastTime = performance.now();

function render(offset, resolution) {

    var time = timer.elapsed();
    saveState();

    if ( ! lastMouse) {
        lastMouse = mouse;
    }

    var down = mouse[2] && ! lastMouse[2];

    if (down) {
        lastMouse = mouse;
    }

    var realTime = performance.now();
    var elapsed = realTime - lastTime;
    lastTime = realTime;
    camera.control(elapsed * .1, [
      pressed('W'), pressed('S'),
      pressed('A'), pressed('D'),
      pressed('R'), pressed('F'),
      pressed('Q'), pressed('E')
    ], mouse.slice(0,2), lastMouse.slice(0,2));

    lastMouse = mouse;

    var config = {
        time: time / 1000,
        mouse: mouse,
        offset: offset,
        resolution: resolution,
        cameraMatrix: camera.view(),
        cameraPosition: camera.position,
        gui: gui.state
    };

    var configJson = JSON.stringify(config);

    if (configJson !== lastConfigJson) {
        if ( ! fpsTimeout) {
            fpsTimeout = setTimeout(function() {
                document.title = frameCount;
                frameCount = 0;
                fpsTimeout = undefined;
            }, 1000);
        }
        frameCount += 1;
        scrubber.value = time;
        drawTriangle(config);
    }

    lastConfigJson = configJson;
}

function play() {
    timer.play();
}

function pause() {
    if (timer.running) {
        timer.pause();
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

function exportGui() {
    var str = JSON.stringify(gui.exportConfig(), undefined, '\t');
    var re = /\[[\s]*([^\s,\]]*,?)[\s]*([^\s,\]]*,?)[\s]*([^\s,\]]*,?)[\s]*\]/g;
    str = str.replace(re, '[$1$2$3]');
    str = str.replace(/,([^\s])/g, ', $1');
    console.log(str);
}

function exportState() {
    var state = getState();
    var str = JSON.stringify(state, undefined, '\t');
    console.log(str);
}

window.play = play;
window.pause = pause;
window.stop = stop;
window.toggle = toggle;
window.stepTo = stepTo;
window.exportGui = exportGui;
window.exportState = exportState;

var canvas = regl._gl.canvas;

mouseChange(canvas, function(buttons, x, y, mods) {
    var lmbPressed = buttons == 1;
    if (lmbPressed) {
        mouse = [x, y, true, 0];
    } else {
        mouse[2] = false;
    }
});

function scrub(evt) {
    pause();
    stepTo(parseFloat(this.value));
}

scrubber.addEventListener('change', scrub);
scrubber.addEventListener('mousedown', function() {
    this.addEventListener('mousemove', scrub);
});
scrubber.addEventListener('mouseup', function() {
    this.removeEventListener('mousemove', scrub);
});


restoreState(stateConf);
restoreState(stateStore.restore('state'));


if (timer.running) {
    play();
}

regl.frame(function() {
    render();
});

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

var captureSetup = function(config) {
    pause();
    canvas.width = config.width;
    canvas.height = config.height;
    canvas.style.width = config.width + 'px';
    canvas.style.height = config.height + 'px';
};

var captureTeardown = function() {
    // Restore your scene as it was before captureSetup
};

var captureRender = function(milliseconds) {
    stepTo(milliseconds);
};

// Default config used by the UI
var captureConfig = {
  fps: 35,
  seconds: 1, // (duration)
  width: 640 * 4,
  height: 360 * 4,
  prefix: 'hlx'
};

var webCapture = new WebCaptureClient(
  canvas, // The canvas element you're drawing to
  captureSetup,
  captureTeardown,
  captureRender,
  captureConfig
);
