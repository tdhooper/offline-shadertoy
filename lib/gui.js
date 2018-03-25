var dat = require('dat.gui');
var WebMidi = require('webmidi');

var GUI = function() {
    this.gui = new dat.GUI();
    this.gui.closed = true;
    this.onChange = function() {};
    this.state = {};
    this.controllers = {};

    this.midi = new Promise(function(resolve, reject) {
        WebMidi.enable(function(error) {
            if (error) {
                reject(error);
            } else {
                resolve(WebMidi);
            }
        });
    });
    this.midiInput = this.midi.then(function(midi) {
        return midi.inputs[1];
    });
    this.midiOutput = this.midi.then(function(midi) {
        return midi.outputs[1];
    });
};

GUI.prototype.initControllers = function(
    state,
    controllers,
    gui,
    config
) {
    if ( ! config) {
        return;
    }
    Object.keys(config).forEach(function(key) {
        var conf = config[key];
        if ( ! Array.isArray(conf)) {
            state[key] = {};
            controllers[key] = {};
            var folder = gui.addFolder(key);
            folder.open();
            this.initControllers(
                state[key],
                controllers[key],
                folder,
                conf
            );
            return;
        }
        conf = conf.slice(); // copy
        state[key] = conf.pop(); // set state to value
        var midiConf;
        if (typeof conf[0] === 'object') {
            midiConf = conf.shift();
        }
        var params = conf.slice();
        params.unshift(state, key); // add global state, and key to start
        var controller = gui.add.apply(gui, params);
        controllers[key] = controller;
        if (midiConf) {
            this.linkMidiControl(controller, conf, midiConf);
        }
    }.bind(this));
};

GUI.prototype.linkMidiControl = function(controller, controlConf, midiConf) {
    var slotConfig = this.midiSlots[midiConf.midi];
    var channel = slotConfig.channel;
    var midiController = slotConfig.controller;

    var onChange = this.setMidiFromController.bind(
        this, midiController, channel, controller, controlConf
    );
    controller.onChange(onChange);
    var lastValue = onChange();

    this.addMidiChangeListener(slotConfig, function(value) {
        var change = value - lastValue;
        change = change === 0 && value === 127 ? 1 : change;
        change = change === 0 && value === 0 ? -1 : change;
        this.incrementController(controller, midiConf, change);
        lastValue = onChange();
    }.bind(this));
};

GUI.prototype.incrementController = function(controller, midiConf, change) {
    var value = this.state[controller.property];
    value += midiConf.step * change;
    controller.setValue(value);
};

GUI.prototype.setMidiFromController = function(midiController, channel, controller, controlConf) {
    var value = this.controllerValueAsMidi(controller, controlConf);
    this.midiOutput.then(function(output) {
        output.sendControlChange(midiController, value, channel);
    });
    return value;
};

GUI.prototype.controllerValueAsMidi = function(controller, config) {
    var min = config[0];
    var max = config[1];
    var value = this.state[controller.property];
    var normal = (value - min) / (max - min);
    return Math.floor(normal * 127);
};

GUI.prototype.addMidiChangeListener = function(slotConfig, listener) {
    this.midiInput.then(function(input) {
        var channel = slotConfig.channel;
        var midiController = slotConfig.controller;
        input.addListener('controlchange', channel, function(evt) {
            if (evt.data[1] == midiController) {
                var value = evt.data[2];
                listener(value);
            }
        });
    });
};


GUI.prototype.exportConfig = function() {
    this.updateConfig(this.controlsConfig, this.state);
    return {
        'showControls': this.gui.closed,
        'controls': this.controlsConfig,
        'midiSlots': this.midiSlots
    };
};

GUI.prototype.loadConfig = function(config) {
    if (config.hasOwnProperty('showControls')) {
        this.gui.closed = config.showControls;
    }
    // if (state.hasOwnProperty('controllers')) {
    //     this.loadControllers(this.controllers, state.controllers);
    // }
    this.midiSlots = config.midiSlots;
    this.controlsConfig = config.controls;
    this.initControllers(
        this.state,
        this.controllers,
        this.gui,
        this.controlsConfig
    );
};

// GUI.prototype.loadControllers = function(controllers, state) {
//     Object.keys(state).forEach(function(key) {
//         if ( ! controllers.hasOwnProperty(key)) {
//             return;
//         }
//         var value = state[key];
//         var controller = controllers[key];
//         if (value instanceof Object) {
//             this.loadControllers(controller, value);
//         } else {
//             controller.setValue(value);
//         }
//     }.bind(this));
// };

// GUI.prototype.exportConfig = function() {
//     this.updateConfig(this.config, this.state);
//     return this.config;
// };

GUI.prototype.updateConfig = function(config, state) {
    if ( ! config) {
        return;
    }
    Object.keys(config).forEach(function(key) {
        var conf = config[key];
        if ( ! Array.isArray(conf)) {
            this.updateConfig(conf, state[key]);
            return;
        }
        conf[conf.length - 1] = state[key];
    }.bind(this));
};


module.exports = GUI;
