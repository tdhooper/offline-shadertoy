import EventEmitter from 'events';

class Timer extends EventEmitter {

    constructor(running, elapsed) {
        super();

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

    elapsed() {
        if ( ! this.running) {
            return this._elapsed;
        }
        var time = performance.now();
        var dt = this.lastTime ? time - this.lastTime : 0;
        this.lastTime = time;
        this._elapsed += dt;
        return this._elapsed;
    }

    play() {
        this.running = true;
        this.lastTime = performance.now();
        this.emit('play');
    }

    pause() {
        this.running = false;
        this.emit('pause');
    }

    stop() {
        this.pause();
        this.reset();
    }

    reset() {
        this._elapsed = 0;
    }

    toggle() {
        if (this.running) {
            this.pause();
        } else {
            this.play();
        }
    }

    set(time) {
        this._elapsed = time;
    }

    serialize() {
        return {
            running: this.running,
            elapsed: this._elapsed
        }
    }

    fromObject(obj) {
        this.running = obj.running;
        this._elapsed = obj.elapsed;
    }
}

export default Timer;
