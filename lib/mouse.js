import mouseChange from 'mouse-change';

function createMouse(canvas) {
  const mouse = [0, 0, false, 0];

  let pub = {
    moveEnabled: true,
  };

  mouseChange(canvas, (buttons, x, y) => {
    if ( ! pub.moveEnabled) {
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
