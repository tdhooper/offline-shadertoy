var Timer = function(running, elapsed) {
    this.running = running || false;
    this._elapsed = elapsed || 0;
    if (this.running) {
        this.play();
    }

    window.play = this.play.bind(this);
    window.pause = this.pause.bind(this);
    window.stop = this.stop.bind(this);
    window.toggle = this.toggle.bind(this);
    window.stepTo = this.set.bind(this);
}

Timer.prototype.elapsed = function() {
    if ( ! this.running) {
        return this._elapsed;
    }
    var time = performance.now();
    var dt = this.lastTime ? time - this.lastTime : 0;
    this.lastTime = time;
    this._elapsed += dt;
    return this._elapsed;
}


Timer.prototype.play = function() {
    this.running = true;
    this.lastTime = performance.now();
}

Timer.prototype.pause = function() {
    this.running = false;
}

Timer.prototype.stop = function() {
    this.pause();
    this.reset();
}

Timer.prototype.reset = function() {
    this._elapsed = 0;
}

Timer.prototype.toggle = function() {
    if (this.running) {
        this.pause();
    } else {
        this.play();
    }
}

Timer.prototype.set = function(time) {
    this._elapsed = time;
}

Timer.prototype.serialize = function() {
    return {
        running: this.running,
        elapsed: this._elapsed
    }
}

Timer.prototype.fromObject = function(obj) {
    this.running = obj.running;
    this._elapsed = obj.elapsed;
}

module.exports = Timer;
