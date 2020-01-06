const EventEmitter = require('events');
const JSRocket = require('../vendor/jsRocket');

class Rocket extends EventEmitter {
  constructor(bpm, rowsPerBeat, values) {
    super();
    this.syncDevice = new JSRocket.SyncDevice();
    this.rowRate = bpm / 60 * rowsPerBeat;
    this.values = values;
    this.ready = false;

    // syncDevice.setConfig({socketURL:"ws://lolcathost:1338"});
    // initialize the connection, default URL is ws://localhost:1338/
    this.syncDevice.init();

    // this is also triggered when the Rocket XML is done, so make sure your ogg is ready
    this.syncDevice.on('ready', this.onSyncReady.bind(this));
    // whenever you change the row, a value or interpolation mode this will get called
    this.syncDevice.on('update', this.onSyncUpdate.bind(this));
    // [Spacebar] in Rocket calls one of those
    this.syncDevice.on('play', this.onPlay.bind(this));
    this.syncDevice.on('pause', this.onPause.bind(this));
  }

  onSyncReady() {
    this.ready = true;
    // jsRocket is done getting all the info you already have in Rocket, or is done parsing the .rocket file
    // this either adds a track to Rocket, or gets it for you
    this.syncValues = Object.assign({}, this.values);
    Object.keys(this.syncValues).forEach((key) => {
      if (Object.prototype.hasOwnProperty.call(this.syncValues, key)) {
        this.syncValues[key] = this.syncDevice.getTrack(key);
      }
    });
  }

  seek(time) {
    if ( ! this.ready) {
      return;
    }
    const row = time * this.rowRate;
    this.syncDevice.update(row);
    this.updateValues(row);
  }

  onSyncUpdate(row) {
    this.updateValues(row);
    // row is only given if you navigate, or change a value on the row in Rocket
    // on interpolation change (hit [i]) no row value is sent, as the current there is the upper row of your block
    const isInterpolationChange = isNaN(row);
    if ( ! isInterpolationChange) {
      this.emit('seek', row / this.rowRate);
    }
  }

  onPlay() {
    this.emit('play');
  }

  onPause() {
    this.emit('pause');
  }

  updateValues(row) {
    Object.keys(this.syncValues).forEach((key) => {
      if (Object.prototype.hasOwnProperty.call(this.syncValues, key)) {
        this.values[key] = this.syncValues[key].getValue(row);
      }
    });
  }
}

module.exports = Rocket;
