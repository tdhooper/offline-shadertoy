var dat = require('dat.gui');
var WebMidi = require('webmidi');

var Control = function(gui, state, key, conf) {
    conf = conf.slice();
    this.min = conf[0];
    this.max = conf[1];
    this.state = state;
    this.key = key;
    this.state[this.key] = conf.pop();
    conf.unshift(this.state, this.key); // add global state, and key to start
    this.controller = gui.add.apply(gui, conf);
};

Control.prototype.onChange = function(listener) {
    this.controller.onChange(listener);
};

Control.prototype.setValue = function(value) {
    return this.controller.setValue(value);
};

Control.prototype.value = function() {
    return this.state[this.key];
};

Control.prototype.midiValue = function() {
    var normal = (this.value() - this.min) / (this.max - this.min);
    return Math.floor(normal * 127);
};


var GUI = function() {
    this.gui = new dat.GUI();
    this.gui.closed = true;
    this.onChange = function() {};
    this.controls = {};
    this.state = {};

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
    prefix,
    controls,
    gui,
    config
) {
    if ( ! config) {
        return;
    }
    Object.keys(config).forEach(function(key) {
        var name = prefix + key[0].toUpperCase() + key.slice(1);
        var conf = config[key];
        if ( ! Array.isArray(conf)) {
            controls[key] = {};
            var folder = gui.addFolder(key);
            folder.open();
            this.initControllers(
                name,
                controls[key],
                folder,
                conf
            );
            return;
        }
        conf = conf.slice(); // copy
        var midiConf;
        if (typeof conf[0] === 'object') {
            midiConf = conf.shift();
        }
        var control = new Control(gui, this.state, name, conf);
        controls[key] = control;
        if (midiConf) {
            this.linkMidiControl(control, midiConf);
        }
    }.bind(this));
};

GUI.prototype.linkMidiControl = function(control, midiConf) {
    var slotConfig = this.midiSlots[midiConf.midi];
    var channel = slotConfig.channel;
    var midiController = slotConfig.controller;

    var onChange = this.setMidiFromController.bind(
        this, midiController, channel, control
    );
    control.onChange(onChange);
    var lastValue = onChange();

    this.addMidiChangeListener(slotConfig, function(value) {
        var change = value - lastValue;
        change = change === 0 && value === 127 ? 1 : change;
        change = change === 0 && value === 0 ? -1 : change;
        this.incrementController(control, midiConf, change);
        lastValue = onChange();
    }.bind(this));
};

GUI.prototype.incrementController = function(control, midiConf, change) {
    var value = control.value() + midiConf.step * change;
    control.setValue(value);
};

GUI.prototype.setMidiFromController = function(midiController, channel, control) {
    var value = control.midiValue();
    this.midiOutput.then(function(output) {
        output.sendControlChange(midiController, value, channel);
    });
    return value;
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
    this.updateConfig(this.controlsConfig, this.controls);
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
        'gui',
        this.controls,
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

GUI.prototype.updateConfig = function(config, controls) {
    if ( ! config) {
        return;
    }
    Object.keys(config).forEach(function(key) {
        var conf = config[key];
        if ( ! Array.isArray(conf)) {
            this.updateConfig(conf, controls[key]);
            return;
        }
        conf[conf.length - 1] = controls[key].value();
    }.bind(this));
};

module.exports = GUI;
