import virtualProject from './rollup-plugin-virtual-project.js';
import glslify from 'rollup-plugin-glslify';

export default {
  plugins: [virtualProject(), glslify()],
  define: {
    process: '({browser: true})',
  }
}