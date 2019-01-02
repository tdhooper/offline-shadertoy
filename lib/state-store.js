
class StateStore {
  constructor(toState, fromState, defaultState) {
    this.toState = toState;
    this.fromState = fromState;

    if (defaultState) {
      this.fromState(defaultState);
    }

    this.defaultState = Object.assign({
      id: 'default',
    }, this.toState());

    if (defaultState) {
      Object.assign(this.defaultState, defaultState);
    }

    this.name = 'state3-' + this.defaultState.id;

    this.restore();

    window.resetState = this.reset.bind(this);
    window.exportState = this.export.bind(this);
  }

  restore() {
    const stateString = localStorage.getItem(this.name);
    const savedState = stateString ? JSON.parse(stateString) : {};
    const state = Object.assign({}, this.defaultState);
    Object.assign(state, savedState);
    this.fromState(state);
  }

  update() {
    this.state = this.toState();
    const stateString = JSON.stringify(this.state);
    localStorage.setItem(this.name, stateString);
    const changed = this.lastStateString !== stateString;
    this.lastStateString = stateString;
    return changed;
  }

  reset() {
    this.fromState(this.defaultState);
  }

  export() {
    const state = Object.assign({}, this.defaultState);
    Object.assign(state, this.toState());
    console.log(JSON.stringify(state, null, 2));
  }
}

module.exports = StateStore;
