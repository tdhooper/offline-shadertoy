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
var EventEmitter = require('events');


class Control extends EventEmitter {

    constructor(name, config, gui) {
        super();
        this.name = name;
        this.value = config.value;
    }

    addUniforms(uniforms, prefix) {
        var name = prefix + this.name[0].toUpperCase() + this.name.slice(1);
        uniforms[name] = this.value;
    }

    createGuiController(gui, name, value, min, max, step) {
        var obj = {};
        obj[name] = value;
        var controller;
        if (min !== undefined && max !== undefined && step !== undefined) {
            controller = gui.add(obj, name, [min, max, step]);
        } else {
            controller = gui.add(obj, name);
        }
        controller.onChange(function() {
            this.value = obj[name];
        }.bind(this));
        return controller;
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





class ToggleControl extends Control {
    constructor(name, config, gui) {
        super(name, config, gui);
        this.guiController = this.createGuiController(
            gui,
            name,
            config.value
        );
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