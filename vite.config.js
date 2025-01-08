import virtualProject from './rollup-plugin-virtual-project.js';

export default {
  plugins: [virtualProject()],
  define: {
    process: '{"browser": true}',
  },
  server: {
    hmr: {
        host: "localhost",
        protocol: "ws",
    },
  }
}
