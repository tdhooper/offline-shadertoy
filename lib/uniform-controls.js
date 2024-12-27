import Controls from './controls';


const create = (config, uniforms) => {

  const controls = new Controls(config);

  controls.addUniforms(uniforms, 'gui');

  return {
    toState: () => {
      const state = {};
      controls.addConfig(state);
      return state;
    },
    fromState: (state) => {
      controls.loadConfig(state);
    },
  };
};


export default create;
