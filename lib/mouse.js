
function createMouse(interactionObject) {
  const mouse = [0, 0, false, 0];

  let pub = {};

  interactionObject.addEventListener('pointerDown', (event) => {
    mouse[0] = event.detail.x;
    mouse[1] = event.detail.y;
    mouse[2] = true;
    mouse[3] = 0;
  });

  interactionObject.addEventListener('pointerUp', () => {
    mouse[2] = false;
  });

  interactionObject.addEventListener('pointerDrag', (event) => {
    mouse[0] = event.detail.x;
    mouse[1] = event.detail.y;
  });

  pub.fromState = (obj) => {
    mouse[0] = obj[0];
    mouse[1] = obj[1];
    mouse[2] = obj[2];
    mouse[3] = obj[3];
  };
  
  pub.toState = () => {
    return mouse.slice();
  };

  return pub;
}

export default createMouse;
