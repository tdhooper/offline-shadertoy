
const createGizmoGui = (save) => {

  const container = document.createElement('div');
  container.classList.add('controls');
  container.classList.add('gizmo-controls');
  document.body.appendChild(container);

  const saveButton = document.createElement('button');
  saveButton.classList.add('gizo-save');
  saveButton.textContent = 'Save';
  container.appendChild(saveButton);

  function saveClicked() {
    save();
  }

  saveButton.addEventListener('click', saveClicked);

  function onDirty() {
    saveButton.classList.add('gizmo-save-dirty');
    saveButton.removeAttribute('disabled');
  }

  function onSaved() {
    saveButton.classList.remove('gizmo-save-dirty');
    saveButton.setAttribute('disabled', null);
  }

  onSaved();

  return {
    dirty: () => {
      onDirty();
    },
    saved: () => {
      onSaved();
    }
  };
};


module.exports = createGizmoGui;
