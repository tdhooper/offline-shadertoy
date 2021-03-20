const fs = require('fs');
const path = require('path');
const through = require('through2');
const glslify = require('glslify');


module.exports = () => {

  function inlineProject(name) {

    const project = {};

    const dir = `./projects/${name}`;
    const files = fs.readdirSync(dir);
    const glsl = files.filter(f => /.(glsl|frag)$/.test(f)).reduce((acc, _file) => {
      const file = path.join(dir, _file);
      let filename = path.parse(file).name;
      if (['shader', 'frag'].indexOf(filename) !== -1) {
        filename = 'main';
      }
      acc[filename] = glslify(file);
      this.emit('file', path.join(__dirname, file)); // watch file
      return acc;
    }, {});

    project.shaders = glsl;

    const configFile = `projects/${name}/config.json`;
    let config = null;
    if (fs.existsSync(configFile)) {
      config = fs.readFileSync(configFile, 'utf8');
      config = JSON.parse(config);
      this.emit('file', path.join(__dirname, configFile));
    }

    project.config = config;

    const drawFile = `projects/${name}/draw.js`;
    if (fs.existsSync(drawFile)) {
      project.draw = '__REQUIRE_DRAW__';
      this.emit('file', path.join(__dirname, drawFile));
    }

    let str = JSON.stringify(project);
    str = str.replace('"__REQUIRE_DRAW__"', `require('./projects/${name}/draw')`);
    return str;
  }

  // Replace LOADPROJECT with a json blob containing it's shaders and config
  function process(buf, enc, next) {

    const re = /LOADPROJECT\('([^\)]+)'\)/;
    let str = buf.toString('utf8');
    let result;

    do {
      result = re.exec(str);
      if (result) {
        const name = result[1];
        str = str.replace(re, inlineProject.bind(this)(name));
      }
    } while (result !== null);

    this.push(str);

    next();
  }

  return through(process);
};
