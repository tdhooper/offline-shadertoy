const mouseChange = require('mouse-change');

function createMouse(canvas) {
  const mouse = [0, 0, false, 0];

  mouseChange(canvas, (buttons, x, y) => {
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

  return mouse;
}

module.exports = createMouse;
