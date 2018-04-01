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
            var input = midi.inputs[1];
            if (input) {
                return input;
            }
            return Promise.reject('No input');
        });

        this.output = midi.then(function(midi) {
            var output = midi.outputs[1];
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

    constructor(config) {
        this.controls = [];

        var gui = new dat.GUI();

        if ( ! config) {
            return;
        }

        Object.keys(config).forEach(function(key) {
            this.add(key, config[key], gui);
        }.bind(this));
    }

    addUniforms(uniforms, prefix) {
        this.controls.forEach(function(control) {
            control.addUniforms(uniforms, prefix);
        });
    }

    add(name, config, gui) {
        if (config.type == 'toggle') {
            this.controls.push(new ToggleControl(name, config, gui));
        }
        if (config.type == 'range') {
            this.controls.push(new RangeControl(name, config, gui));
        }
        if (config.type == 'rangeloop') {
            this.controls.push(new RangeLoopControl(name, config, gui));
        }
    }
}



class ControlGroup extends Controls {

    constructor(name, config, gui) {
        this.name = name;
        var folder = gui.addFolder(name);
        super(config, folder);
    }

    addUniforms(uniforms, prefix) {
        prefix = prefix + this.name[0].toUpperCase() + this.name.slice(1);
        this.controls.forEach(function(control) {
            control.addUniforms(uniforms, prefix);
        });
    }
}



class Control extends EventEmitter {

    constructor(name, config, gui) {
        super();
        this.name = name;
        this.value = config.value;
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
        uniforms[name] = this.value;
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
        super(name, config, gui);

        this.createGuiController(
            gui,
            name,
            config.value,
            config.min,
            config.max,
            config.step
        );

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
            this.midiUpdateValue(this.value + config.step * delta);
        }.bind(this));
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