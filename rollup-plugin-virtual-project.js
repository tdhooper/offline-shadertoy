import fs from 'fs';
import path from 'path';
import glslify from 'glslify';

export default function virtualProject() {

  let pathRegex = /([^\.]+)\/virtual-project/;

  return {
    name: 'rollup-plugin-virtual-project',
    resolveId ( source ) {
      if (source.match(pathRegex)) {
        // this signals that Rollup should not ask other plugins or check
        // the file system to find this id
        return source;
      }
      return null; // other ids should be handled as usually
    },
    load ( id ) {
      let matches = id.match(pathRegex);
      if (matches) {
        let absoluteDir = matches[1];
        const relativeDir = `.${absoluteDir}`;
        
        let project = {};

        const files = fs.readdirSync(relativeDir);
        const glsl = files.filter(f => /.(glsl|frag)$/.test(f)).reduce((acc, _file) => {
          const file = path.join(relativeDir, _file);
          let filename = path.parse(file).name;
          if (['shader', 'frag'].indexOf(filename) !== -1) {
            filename = 'main';
          }
          acc[filename] = {
            file: file,
            glsl: glslify(file, {basedir: './'}),
          };
          //this.emit('file', path.join(__dirname, file)); // watch file
          return acc;
        }, {});

        project.shaders = glsl;

        const configFile = `${relativeDir}/config.json`;
        let config = null;
        if (fs.existsSync(configFile)) {
          config = fs.readFileSync(configFile, 'utf8');
          config = JSON.parse(config);
          //this.emit('file', path.join(__dirname, configFile));
        }

        project.config = config;

        const drawFile = `${relativeDir}/draw.js`;
        let hasDrawFile = fs.existsSync(drawFile);

        let moduleSource = [];

        if (hasDrawFile) {
          moduleSource.push(`import draw from '${absoluteDir}/draw.js'`);
        }

        moduleSource.push(`let project = ${JSON.stringify(project)};`)

        if (hasDrawFile) {
          moduleSource.push(`project.draw = draw;`);
        } else {
          moduleSource.push(`project.draw = null;`);
        }

        moduleSource.push(`export default project;`);
        return moduleSource.join('\n');
      }
      return null; // other ids should be handled as usually
    }
  };
}