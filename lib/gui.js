var dat = require('dat.gui');

var GUI = function(config, onChange) {
    this.gui = new dat.GUI();
    this.gui.closed = true;

    this.state = {};
    this.controllers = {};

    Object.keys(config).forEach(function(key) {
        var conf = config[key];
        this.state[key] = conf.pop();
        conf.unshift(this.state, key);
        var controller = this.gui.add.apply(this.gui, conf);
        this.controllers[key] = controller;
        controller.onChange(onChange);
    }.bind(this));
};

GUI.prototype.saveState = function() {
    return {
        'closed': this.gui.closed,
        'controllers': this.state
    };
};

GUI.prototype.loadState = function(newState) {
    if (newState.hasOwnProperty('closed')) {
        this.gui.closed = newState.closed;
    }
    var controllers = newState.controllers;
    if ( ! controllers) {
        return;
    }
    Object.keys(controllers).forEach(function(key) {
        if (this.controllers.hasOwnProperty(key)) {
            this.controllers[key].setValue(controllers[key]);
        }
    }.bind(this));
};

module.exports = GUI;
