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
  let gizmoAdjustmentT = req.body.gizmoAdjustmentT;
  let gizmoAdjustmentR = req.body.gizmoAdjustmentR;
  let gizmoAdjustmentS = req.body.gizmoAdjustmentS;

  if (empty(files)) {
    res.statusCode = 500;
    res.end('No files specified');
    return;
  }

  if (empty(gizmoAdjustmentT) || empty(gizmoAdjustmentR) || empty(gizmoAdjustmentS)) {
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

  const transformArgsGlsl = `vec3(${gizmoAdjustmentT.join(',')}), vec4(${gizmoAdjustmentR.join(',')}), vec3(${gizmoAdjustmentS.join(',')})`;

  // this doesn't cope with comments or macros
  let findClosingBracket = (source, startIndex) => {
    let counter = 0;
    let index = startIndex;
    while (index < source.length) {
      let char = source.charAt(index);
      if (char == "(") {
        counter++;
      }
      if (char == ")") {
        counter--;
      }
      if (counter < 0) {
        return index;
      }
      index++;
    }
    return startIndex;
  }

  let findReplacements = (source) => {
    let reMethod = /egTransform[\s\n]*\([^,)]*/g;
    let match;
    let replacements = [];
    while((match = reMethod.exec(source)) !== null) {
      let start = reMethod.lastIndex;
      let end = findClosingBracket(source, start);
      replacements.push({start, end});
    }
    return replacements;
  }

  files.forEach(file => {
    let source = fs.readFileSync(file, 'utf8');
    let replacements = findReplacements(source);
    if (replacements.length > 0) {
      replacements.reverse();
      replacements.forEach(replacement => {
        console.log(replacement);
        const insert = `, ${transformArgsGlsl}`;
        source = source.slice(0, replacement.start) + insert + source.slice(replacement.end);
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