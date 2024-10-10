const budo = require('budo');
const bodyParser = require('body-parser');
const empty = require('is-empty');
const Router = require('router');
const fs = require('node:fs');
const path = require('path');

var router = Router();

budo('./index.js', {
  live: true,
  stream: process.stdout,
  ssl: true,
  css: '/styles/main.css',
  middleware: [
    bodyParser.json({
      limit: 1024 * 1024
    }),
    router
  ]
});


router.post('/save-gizmo', function(req, res) {

  if (empty(req.body)) {
    res.statusCode = 500;
    res.end('Missing content');
    return;
  }

  let files = req.body.files;
  let matrix = req.body.matrix;

  if (empty(files)) {
    res.statusCode = 500;
    res.end('No files specified');
    return;
  }

  if (empty(matrix)) {
    res.statusCode = 500;
    res.end('No matrix specified');
    return;
  }

  const projectsPath = path.parse(path.normalize('./projects/'));

  files = files.filter((file) => {
    let parsed = path.parse(path.normalize(file));
    if (parsed.dir.indexOf(projectsPath.dir) == 0) {
      return true;
    }
    console.log(`${file} directory ${parsed.dir} outside of projects directory ${projectsPath.dir}`);
  });

  if (empty(files)) {
    res.statusCode = 500;
    res.end('No valid files');
    return;
  }

  const reGizo = /GIZMO[\s\n]*\(([^)]+)\)[\s\n]*[;,\,]/g;
  const matrixGlsl = `mat4(${matrix.join(',')})`

  files.forEach(file => {
    let source = fs.readFileSync(file, 'utf8');
    let match;
    let replacements = [];
    while((match = reGizo.exec(source)) !== null) {
      replacements.push({
        index: match.index,
        p: match[1],
      })
    }
    if (replacements.length > 0) {
      replacements.reverse();
      replacements.forEach(replacement => {
        const insert = `GIZMO_TRANSFORM(${replacement.p}, ${matrixGlsl});\n`;
        source = source.slice(0, replacement.index) + insert + source.slice(replacement.index);
      });
      fs.writeFileSync(file, source);
    }
  });

  res.statusCode = 200;
  res.end('Success');
});

router.use(function (err, req, res, next) {
  console.log(err.message);
  res.statusCode = 500;
  res.end('Error');
})