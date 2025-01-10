import Controls from './controls';


const create = (config, uniforms) => {

  const controls = new Controls(config);

  return {
    toState: () => {
      const state = {};
      controls.addConfig(state);
      return state;
    },
    fromState: (state) => {
      controls.loadConfig(state);
    },
    addUniforms: (uniforms) => {
      controls.addUniforms(uniforms, 'gui');
    },
  };
};


export default create;
