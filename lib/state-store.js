function save(name, state) {
    localStorage.setItem(name, JSON.stringify(state))
}

function restore(name) {
    var stateStr = localStorage.getItem(name);
    return stateStr && JSON.parse(stateStr);
}

module.exports = {
    save: save,
    restore: restore
};
