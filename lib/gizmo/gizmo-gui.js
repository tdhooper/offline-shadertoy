
const createGizmoGui = (save, setMode) => {

  const container = document.createElement('div');
  container.classList.add('controls');
  container.classList.add('gizmo-controls');
  document.body.appendChild(container);

  const tButton = document.createElement('button');
  tButton.textContent = 'T';
  container.appendChild(tButton);
  const rButton = document.createElement('button');
  rButton.textContent = 'R';
  container.appendChild(rButton);
  const sButton = document.createElement('button');
  sButton.textContent = 'S';
  container.appendChild(sButton);

  const saveButton = document.createElement('button');
  saveButton.classList.add('gizo-save');
  saveButton.textContent = 'Save';
  container.appendChild(saveButton);

  function saveClicked() {
    save();
  }

  saveButton.addEventListener('click', saveClicked);
  tButton.addEventListener('click', () => setMode('translate'));
  rButton.addEventListener('click', () => setMode('rotate'));
  sButton.addEventListener('click', () => setMode('scale'));

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


export default createGizmoGui;
