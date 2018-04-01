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


class Controls {

    constructor(config) {
        this.controls = [];

        var gui = new dat.GUI();

        var midi = new Promise(function(resolve, reject) {
            WebMidi.enable(function(error) {
                if (error) {
                    reject(error);
                } else {
                    resolve(WebMidi);
                }
            });
        });

        var midiInput = midi.then(function(midi) {
            var input = midi.inputs[1];
            if (input) {
                return input;
            }
            return Promise.reject('No input');
        });

        var midiOutput = midi.then(function(midi) {
            var output = midi.outputs[1];
            if (output) {
                return output;
            }
            return Promise.reject('No output');
        });

        var _midi = {
            input: midiInput,
            output: midiOutput
        };

        if ( ! config) {
            return;
        }

        Object.keys(config).forEach(function(key) {
            this.add(key, config[key], gui, _midi);
        }.bind(this));
    }

    addUniforms(uniforms, prefix) {
        this.controls.forEach(function(control) {
            control.addUniforms(uniforms, prefix);
        });
    }

    add(name, config, gui, midi) {
        if (config.type == 'toggle') {
            this.controls.push(new ToggleControl(name, config, gui, midi));
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

    constructor(name, config, gui, midi) {
        super();
        this.name = name;
        this.midi = midi;
        this.value = config.value;
    }

    guiUpdateValue(value) {
        this.value = value;
        this.updateMidi();
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
            controller = gui.add(guiObj, name, [min, max, step]);
        } else {
            controller = gui.add(guiObj, name);
        }
        controller.onChange(function() {
            this.guiUpdateValue(guiObj[name]);
        }.bind(this));
        this.controller = controller;
    }

    updateMidi() {}
}



class ToggleControl extends Control {
    constructor(name, config, gui, midi) {
        super(name, config, gui, midi);

        this.createGuiController(
            gui,
            name,
            config.value
        );

        var note = config.note;
        if ( ! note) {
            return;
        }
        this.note = note;
        midi.input.then(function(input) {
            input.addListener('noteoff', 'all', function(evt) {
                if (evt.note.name + evt.note.octave == note) {
                    this.midiUpdateValue( ! this.value);
                }
            }.bind(this));
        }.bind(this));

        this.updateMidi();
    }

    updateMidi() {
        if ( ! this.note) {
            return;
        }
        this.midi.output.then(function(output) {
            if (this.value) {
                output.playNote(this.note);
            } else {
                output.stopNote(this.note);
            }
        }.bind(this));
    }
}




// var RangeControl = function(name, config, gui) {
//     Control.apply(this, arguments);
//     this.guiController = this.createGuiController(
//         gui,
//         name,
//         config.value,
//         config.min,
//         config.max,
//         config.step
//     );
// };

// RangeControl.prototype = Object.create(Control.prototype);




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