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

Control.prototype.setValue = function(value, loop) {
    if (loop) {
        value = value > this.max ? this.min + (value - this.max) : value;
        value = value < this.min ? this.max - (value - this.min) : value;
    }
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
        var input = midi.inputs[1];
        if (input) {
            return input;
        }
        return Promise.reject('No input');
    });
    this.midiOutput = this.midi.then(function(midi) {
        var output = midi.outputs[1];
        if (output) {
            return output;
        }
        return Promise.reject('No output');
    });
};

GUI.prototype.dialButtomMap = {
    11: 'C0',
    12: 'C#0',
    13: 'D0',
    14: 'D#0',
    15: 'E0',
    16: 'F0',
    17: 'F#0',
    18: 'G0'
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

/*

control

midiconf

if note
    listen to note on, toggle
    listen to gui, midiValue/value, update button

if control
    listen to cc, increment value, update collar
    listen to gui, update collar

if control loop
    ADD three gui controls, value, speed, autoToggle 
    listen to cc
        if ! autoToggle increment value (loop), update collar
        if autoToggle increment speed, update collar
    listen to note on, toggle autoToggle
*/

GUI.prototype.linkMidiControl = function(control, midiConf) {
    var changeHandler = this.setMidiFromControl.bind(this, midiConf, control);
    control.onChange(changeHandler);
    var lastValue = changeHandler();

    this.addMidiChangeListener(midiConf, function(evt) {
        if (evt.type === 'pot') {
            var value = evt.value;
            var change = value - lastValue;
            change = change === 0 && value === 127 ? 1 : change;
            change = change === 0 && value === 0 ? -1 : change;
            this.incrementController(control, midiConf, change);
            lastValue = changeHandler();
        }
        if (evt.type === 'note') {
            control.setValue( ! control.value());
        }
    }.bind(this));
};

GUI.prototype.setMidiFromControl = function(midiConf, control) {
    var value = control.midiValue();
    this.midiOutput.then(function(output) {
        if (midiConf.hasOwnProperty('controller')) {
            output.sendControlChange(midiConf.controller, value);
        }
        if (midiConf.hasOwnProperty('note')) {
            if (control.value()) {
                output.playNote(midiConf.note);
            } else {
                output.stopNote(midiConf.note);
            }
            
        }
    });
    return value;
};

GUI.prototype.addMidiChangeListener = function(midiConf, listener) {
    this.midiInput.then(function(input) {
        if (midiConf.hasOwnProperty('controller')) {
            var midiController = midiConf.controller;
            input.addListener('controlchange', 'all',function(evt) {
                if (evt.data[1] == midiController) {
                    var value = evt.data[2];
                    listener({
                        type: 'pot',
                        value: value
                    });
                }
            });
        }
        if (midiConf.hasOwnProperty('note')) {
            var note = midiConf.note;
            input.addListener('noteoff', 'all', function(evt) {
                if (evt.note.name + evt.note.octave == note) {
                    listener({
                        type: 'note',
                        value: false
                    });
                }
            });
        }
    });
};

GUI.prototype.incrementController = function(control, midiConf, change) {
    var value = control.value() + midiConf.step * change;
    control.setValue(value, midiConf.loop);
};

GUI.prototype.exportConfig = function() {
    this.updateConfig(this.controlsConfig, this.controls);
    return {
        'showControls': this.gui.closed,
        'controls': this.controlsConfig
    };
};

GUI.prototype.loadConfig = function(config) {
    if (config.hasOwnProperty('showControls')) {
        this.gui.closed = config.showControls;
    }
    // if (state.hasOwnProperty('controllers')) {
    //     this.loadControllers(this.controllers, state.controllers);
    // }
    this.controlsConfig = config.controls;
    this.initControllers(
        'gui',
        this.controls,
        this.gui,
        this.controlsConfig
    );
};

GUI.prototype.exportState = function() {
    return {
        'showControls': this.gui.closed,
        'controls': this.state,
    };
};

GUI.prototype.loadState = function(state) {
    if (state.hasOwnProperty('showControls')) {
        this.gui.closed = state.showControls;
    }
    Object.keys(this.state).forEach(function(key) {
        this.state[key] = state.controls[key];
    }.bind(this));
};

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
