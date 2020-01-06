const EventEmitter = require('events');

class Scrubber extends EventEmitter {
  constructor(timer) {
    super();

    this.timer = timer;
    this.lastTime = null;

    const controls = document.createElement('div');
    controls.classList.add('controls');
    document.body.appendChild(controls);

    this.scrubber = document.createElement('input');
    this.scrubber.classList.add('scrubber');
    this.scrubber.setAttribute('type', 'range');
    this.scrubber.min = 0;
    this.scrubber.max = 3500; // milliseconds
    this.scrubber.step = 10;
    controls.appendChild(this.scrubber);

    this.scrubber.addEventListener('change', this.scrub.bind(this));
    this.scrubber.addEventListener('mousedown', this.startScrub.bind(this));
    this.scrubber.addEventListener('mouseup', this.stopScrub.bind(this));
  }

  scrub() {
    this.timer.set(parseFloat(this.scrubber.value));
    this.emit('scrub', this.scrubber.value);
  }

  startScrub() {
    this.timer.pause();
    this.scrubber.addEventListener('mousemove', this.scrub.bind(this));
  }

  stopScrub() {
    this.scrubber.removeEventListener('mousemove', this.scrub.bind(this));
  }

  update() {
    const time = this.timer.elapsed();
    if (this.lastTime !== time) {
      this.scrubber.value = time;
    }
    this.lastTime = time;
  }
}

module.exports = Scrubber;
