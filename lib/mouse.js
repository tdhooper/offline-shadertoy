const mouseChange = require('mouse-change');

function createMouse(canvas) {
  const mouse = [0, 0, false, 0];

  let interface = {
    moveEnabled: true,
  };

  mouseChange(canvas, (buttons, x, y) => {
    if ( ! interface.moveEnabled) {
      return;
    }
    const lmbPressed = buttons === 1;
    if (lmbPressed) {
      mouse[0] = x;
      mouse[1] = y;
      mouse[2] = true;
      mouse[3] = 0;
    } else {
      mouse[2] = false;
    }
  });

  interface.fromState = (obj) => {
    mouse[0] = obj[0];
    mouse[1] = obj[1];
    mouse[2] = obj[2];
    mouse[3] = obj[3];
  };
  
  interface.toState = () => {
    return mouse.slice();
  };

  return interface;
}

module.exports = createMouse;
