
const createScrubber = (container, timer) => {
  const playpause = document.createElement('button');
  playpause.classList.add('playpause');
  container.appendChild(playpause);

  const scrubber = document.createElement('input');
  scrubber.classList.add('scrubber');
  scrubber.setAttribute('type', 'range');
  scrubber.min = 0;
  scrubber.max = 6000; // milliseconds
  scrubber.step = 10;
  container.appendChild(scrubber);

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

  function togglePlaypause() {
    timer.toggle();
  }

  playpause.addEventListener('click', togglePlaypause);

  function onPlay() {
    playpause.classList.remove('playpuse-play');
    playpause.classList.add('playpuse-pause');
    playpause.textContent = 'Pause';   
  }

  function onPause() {
    playpause.classList.remove('playpuse-pause');
    playpause.classList.add('playpuse-play');
    playpause.textContent = 'Play';
  }

  timer.addListener('play', onPlay);
  timer.addListener('pause', onPause);

  if (timer.running) {
    onPlay();
  } else {
    onPause();
  }

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
