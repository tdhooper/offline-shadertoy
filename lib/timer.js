var Timer = function(running, elapsed) {
    this.running = running || false;
    this._elapsed = elapsed || 0;
    if (this.running) {
        this.play();
    }
}

Timer.prototype.elapsed = function() {
    if ( ! this.running) {
        return this._elapsed;
    }
    var time = performance.now();
    var dt = time - this.lastTime;
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

Timer.prototype.reset = function() {
    this._elapsed = 0;
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

Timer.fromObject = function(obj) {
    return new Timer(obj.running, obj.elapsed);
}

module.exports = Timer;
