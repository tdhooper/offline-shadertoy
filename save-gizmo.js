import empty from 'is-empty';
import fs from 'node:fs';
import path from 'path';

export default async function saveGizmo(req, res, next) {

  res.set({ 'Content-Type': 'text/html' });

  if (empty(req.body)) {
    res.statusCode = 500;
    res.end('Missing content');
    return;
  }

  let files = req.body.files;
  let gizmoAdjustment = req.body.gizmoAdjustment;

  if (empty(files)) {
    res.statusCode = 500;
    res.end('No files specified');
    return;
  }

  if (empty(gizmoAdjustment)) {
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

  let roundVec = (vec, dp) => {
    let r = Math.pow(10, dp);
    return vec.map(v => { return Math.round(v * r) / r });
  }

  const transformArgsGlsl = ([
    `vec3(${roundVec(gizmoAdjustment.t, 7).join(',')})`,
    `vec4(${roundVec(gizmoAdjustment.r, 7).join(',')})`,
    `vec3(${roundVec(gizmoAdjustment.s, 7).join(',')})`,
  ]).join(', ');

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
};
