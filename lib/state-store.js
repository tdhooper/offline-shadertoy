
class StateStore {
  constructor(toState, fromState, defaultState) {
    this.toState = toState;
    this.fromState = fromState;
    this.defaultState = defaultState;
    this.name = 'StateStore_' + defaultState.name;
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
    const state = this.toState();
    const stateString = JSON.stringify(state);
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
