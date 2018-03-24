var dat = require('dat.gui');

var GUI = function() {
    this.gui = new dat.GUI();
    this.gui.closed = true;
    this.onChange = function() {};
    this.state = {};
    this.controllers = {};
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
        conf = conf.slice();
        state[key] = conf.pop();
        conf.unshift(state, key);
        var controller = gui.add.apply(gui, conf);
        controllers[key] = controller;
        controller.onChange(this.onChange);
    }.bind(this));
};

GUI.prototype.exportConfig = function() {
    this.updateConfig(this.controlsConfig, this.state);
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
