const fs = require('fs');
const path = require('path');
const through = require('through2');
const glslify = require('glslify');


module.exports = () => {

  function inlineProject(name) {

    const fragFile = ['shader', 'frag']
      .map(fragName => `./projects/${name}/${fragName}.glsl`)
      .find(fs.existsSync);
    const frag = glslify(fragFile);
    this.emit('file', path.join(__dirname, fragFile));

    const configFile = `projects/${name}/config.json`;
    let config = null;
    if (fs.existsSync(configFile)) {
      config = fs.readFileSync(configFile, 'utf8');
      config = JSON.parse(config);
      this.emit('file', path.join(__dirname, configFile));
    }

    const project = {
      config,
      frag,
    };

    const drawFile = `projects/${name}/draw.js`;
    if (fs.existsSync(drawFile)) {
      project.draw = '__REQUIRE_DRAW__';
      this.emit('file', path.join(__dirname, drawFile));
    }

    let str = JSON.stringify(project);
    str = str.replace('"__REQUIRE_DRAW__"', `require('./projects/${name}/draw')`);
    return str;
  }

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
