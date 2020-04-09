/*

midiconf

if TOGGLE
    ADD boolen control
    listen to note on, toggle
    listen to gui, midiValue/value, update button

if RANGE
    ADD slider control
    listen to cc, increment value, update collar
    listen to gui, update collar

if LOOP
    ADD three gui controls
        value slider
        speed slider
        auto boolean 
    listen to cc
        if ! autoToggle increment value (loop), update collar
        if autoToggle increment speed, update collar
    listen to note on, toggle autoToggle
*/

var dat = require('dat.gui');
var WebMidi = require('webmidi');
var EventEmitter = require('events');
const cloneDeep = require('clone-deep');
const hotkeys = require('hotkeys-js');


class MidiControl extends EventEmitter {
    constructor() {
        super();

        var midi = new Promise(function(resolve, reject) {
            WebMidi.enable(function(error) {
                if (error) {
                    reject(error);
                } else {
                    resolve(WebMidi);
                }
            });
        });

        this.input = midi.then(function(midi) {
            var input = midi.getInputByName('X-TOUCH MINI');
            if (input) {
                return input;
            }
            return Promise.reject('No input');
        });

        this.output = midi.then(function(midi) {
            var output = midi.getOutputByName('X-TOUCH MINI');
            if (output) {
                return output;
            }
            return Promise.reject('No output');
        });
    }
}

class ToggleMidiControl extends MidiControl {

    constructor(value, note) {
        super();

        this.note = note;
        this.setValue(value);

        this.input.then(function(input) {
            input.addListener('noteoff', 'all',function(evt) {
                if (evt.note.name + evt.note.octave == note) {
                    this.emit('press');
                }
            }.bind(this));
        }.bind(this));
    }

    setValue(on) {
        this.output.then(function(output) {
            if (on) {
                output.playNote(this.note);
            } else {
                output.stopNote(this.note);
            }
        }.bind(this));
    }
}


class RotaryMidiControl extends MidiControl {

    constructor(value, controller, min, max) {
        super();

        this.controller = controller;
        this.min = min;
        this.max = max;

        this.setValue(value);

        this.input.then(function(input) {
            input.addListener('controlchange', 'all',function(evt) {
                if (evt.data[1] == controller) {
                    var value = evt.data[2];

                    var change = value - this.value;
                    change = change === 0 && value === 127 ? 1 : change;
                    change = change === 0 && value === 0 ? -1 : change;

                    this.emit('change', change);
                }
            }.bind(this));
        }.bind(this));
    }

    setValue(value) {
        var normal = (value - this.min) / (this.max - this.min);
        this.value = Math.floor(normal * 127);
        this.output.then(function(output) {
           output.sendControlChange(this.controller, this.value);
        }.bind(this));
    }
}


class Controls {

    constructor(config, gui) {
        config = cloneDeep(config);
        this.controls = [];
        this.config = config;
        gui = gui || new dat.GUI();
        this.gui = gui;

        if ( ! config) {
            return;
        }

        gui.closed = !! config.closed;

        Object.keys(config).forEach(function(key) {
            this.add(key, config[key], gui);
        }.bind(this));
    }

    addUniforms(uniforms, prefix) {
        this.controls.forEach(function(control) {
            control.addUniforms(uniforms, prefix);
        });
    }

    addConfig(config) {
        if (this.gui.closed) {
            config.closed = true;
        }
        this.controls.forEach(function(control) {
            control.addConfig(config);
        });
    }

    loadConfig(config) {
        this.gui.closed = !! config.closed;
        this.controls.forEach(function(control) {
            control.loadConfig(config);
        });
    }

    add(name, config, gui) {
        if (Array.isArray(config)) {
            var midiConf;
            if (typeof config[0] == 'object') {
                midiConf = config.shift(0);
            }
            if (config.length == 1) {
                config = {
                    type: 'toggle',
                    value: config[0]
                };
            } else {
                config = {
                    type: 'range',
                    min: config[0],
                    max: config[1],
                    step: Math.abs(config[0] - config[1]) / 1280,
                    value: config[2]
                };
            }
            if (midiConf && midiConf.hasOwnProperty('step')) {
                config.step = midiConf.step;
            }
        }

        if (config.type == 'toggle') {
            this.controls.push(new ToggleControl(name, config, gui));
            return;
        }
        if (config.type == 'range') {
            this.controls.push(new RangeControl(name, config, gui));
            return;
        }
        if (config.type == 'rangeloop') {
            this.controls.push(new RangeLoopControl(name, config, gui));
            return;
        }
        if (typeof config == 'object') {
            this.controls.push(new ControlGroup(name, config, gui));
        }
    }
}



class ControlGroup extends Controls {

    constructor(name, config, gui) {
        var folder = gui.addFolder(name);
        super(config, folder);
        this.name = name;
    }

    addUniforms(uniforms, prefix) {
        prefix = prefix + this.name[0].toUpperCase() + this.name.slice(1);
        super.addUniforms(uniforms, prefix);
    }

    addConfig(config) {
        config[this.name] = {};
        super.addConfig(config[this.name]);
    }

    loadConfig(config) {
        super.loadConfig(config[this.name]);
    }
}



class Control extends EventEmitter {

    constructor(name, config, gui) {
        super();
        this.config = config;
        this.name = name;
        this.value = config.value;
    }

    keyboardUpdateValue(value) {
        this.value = value;
        this.controller.setValue(value);
        if (this.midiControl) {
            this.midiControl.setValue(value);
        }
    }

    guiUpdateValue(value) {
        this.value = value;
        if (this.midiControl) {
            this.midiControl.setValue(value);
        }
    }

    midiUpdateValue(value) {
        this.value = value;
        this.controller.setValue(value);
    }

    addUniforms(uniforms, prefix) {
        var name = prefix + this.name[0].toUpperCase() + this.name.slice(1);
        uniforms[name] = () => this.value;
    }

    addConfig(config) {
        this.config.value = this.value;
        config[this.name] = this.config;
    }

    loadConfig(config) {
        this.value = config[this.name].value;
        this.controller.setValue(this.value);
        if (this.midiControl) {
            this.midiControl.setValue(this.value);
        }
    }

    createGuiController(gui, name, value, min, max, step) {
        var guiObj = {};
        guiObj[name] = value;
        var controller;
        if (min !== undefined && max !== undefined && step !== undefined) {
            controller = gui.add(guiObj, name, min, max, step);
        } else {
            controller = gui.add(guiObj, name);
        }
        controller.onChange(function() {
            this.guiUpdateValue(guiObj[name]);
        }.bind(this));
        this.controller = controller;
    }
}



class ToggleControl extends Control {
    constructor(name, config, gui) {
        super(name, config, gui);

        this.createGuiController(
            gui,
            name,
            config.value
        );

        if ( ! config.note) {
            return;
        }

        this.midiControl = new ToggleMidiControl(
            config.value,
            config.note
        );
        this.midiControl.on('press', function() {
            this.midiUpdateValue( ! this.value);
        }.bind(this));
    }
}




class RangeControl extends Control {
    constructor(name, config, gui) {

        if (config.step == undefined) {
            config.step = Math.abs(config.max - config.min) / 255;
        }

        super(name, config, gui);

        this.createGuiController(
            gui,
            name,
            config.value,
            config.min,
            config.max,
            config.step
        );

        function hotkeyAdjust(delta) {
            var value = this.adjustValue(delta);
            this.keyboardUpdateValue(value);
        }

        if (config.hotkeyUp) {
            hotkeys(config.hotkeyUp, hotkeyAdjust.bind(this, 1));
        }

        if (config.hotkeyDown) {
            hotkeys(config.hotkeyDown, hotkeyAdjust.bind(this, -1));
        }

        if ( ! config.controller) {
            return;
        }

        this.midiControl = new RotaryMidiControl(
            config.value,
            config.controller,
            config.min,
            config.max
        );
        this.midiControl.on('change', function(delta) {
            var value = this.adjustValue(delta);
            this.midiUpdateValue(value);
        }.bind(this));
    }

    adjustValue(delta) {
        var min = this.config.min;
        var max = this.config.max;
        var value = this.value + this.config.step * delta;
        if (this.config.loop) {
            value = value > max ? min + (value - max) : value;
            value = value < min ? max - (value - min) : value;
        } else {
            value = Math.max(value, min);
            value = Math.min(value, max);
        }
        return value;
    }
}




// var RangeLoopControl = function(config) {
//     var groupConfig = {};
//     groupConfig.value = {
//         type: 'range',
//         min: config.min,
//         max: config.max,
//         step: config.step,
//         value: config.value
//     };
//     groupConfig.speed = {
//         type: 'range',
//         min: config.min,
//         max: config.max,
//         step: config.step,
//         value: config.value
//     };
//     groupConfig.auto = {
//         type: 'toggle',
//         value: config.auto
//     };
//     ControlGroup.apply(this, groupConfig);
// };

// RangeLoopControl.prototype = Object.create(ControlGroup.prototype);

// // var parent = DiscloseDropdown.prototype;

// // DiscloseCollapsingNav.prototype.start = function() {
// //     parent.start.call(this);

// //     this.updateSubscription = this.app.eventMediator.subscribe('collapsingNav.beforeUpdate', this.close.bind(this));
// // };



module.exports = Controls;