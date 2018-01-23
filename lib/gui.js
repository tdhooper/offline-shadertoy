var dat = require('dat.gui');

var GUI = function(config, onChange) {
    this.gui = new dat.GUI();
    this.gui.closed = true;
    this.onChange = onChange;
    this.state = {};
    this.controllers = {};
    this.initControllers(
        this.state,
        this.controllers,
        this.gui,
        config
    );
};

GUI.prototype.initControllers = function(
    state,
    controllers,
    gui,
    config
) {
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
        state[key] = conf.pop();
        conf.unshift(state, key);
        var controller = gui.add.apply(gui, conf);
        controllers[key] = controller;
        controller.onChange(this.onChange);
    }.bind(this));
};

GUI.prototype.saveState = function() {
    return {
        'closed': this.gui.closed,
        'controllers': this.state
    };
};

GUI.prototype.loadState = function(state) {
    if (state.hasOwnProperty('closed')) {
        this.gui.closed = state.closed;
    }
    if (state.hasOwnProperty('controllers')) {
        this.loadControllers(this.controllers, state.controllers);
    }
};

GUI.prototype.loadControllers = function(controllers, state) {
    Object.keys(state).forEach(function(key) {
        if ( ! controllers.hasOwnProperty(key)) {
            return;
        }
        var value = state[key];
        var controller = controllers[key];
        if (value instanceof Object) {
            this.loadControllers(controller, value);
        } else {
            controller.setValue(value);
        }
    }.bind(this));
};

module.exports = GUI;
