
const createScrubber = (timer) => {
  const controls = document.createElement('div');
  controls.classList.add('controls');
  document.body.appendChild(controls);

  const scrubber = document.createElement('input');
  scrubber.classList.add('scrubber');
  scrubber.setAttribute('type', 'range');
  scrubber.min = 0;
  scrubber.max = 8000; // milliseconds
  scrubber.step = 10;
  controls.appendChild(scrubber);

  function scrub() {
    timer.set(parseFloat(scrubber.value));
  }

  function startScrub() {
    timer.pause();
    scrubber.addEventListener('mousemove', scrub);
  }

  function stopScrub() {
    scrubber.removeEventListener('mousemove', scrub);
  }

  scrubber.addEventListener('change', scrub);
  scrubber.addEventListener('mousedown', startScrub);
  scrubber.addEventListener('mouseup', stopScrub);

  let lastTime = null;

  return {
    update: () => {
      const time = timer.elapsed();
      if (lastTime !== time) {
        scrubber.value = time;
      }
      lastTime = time;
    },
  };
};


module.exports = createScrubber;
