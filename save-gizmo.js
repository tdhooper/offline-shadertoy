import empty from 'is-empty';
import fs from 'node:fs';
import path from 'path';
import gizmoShaderModifier from './lib/gizmo/gizmo-shader-modifier.js'

export default function saveGizmo(vite) {
  
  return async function post(req, res, next) {

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

    let writeFile = async (file, content) => {
      // Stop watching the file so the page doesn't reload when we write to it
      await vite.watcher.unwatch(path.resolve(file));

      // Write the file
      fs.writeFileSync(file, content);
      
      // Start watching the file again
      vite.watcher.add(path.resolve(file));

      // Invalidate the cached file, so vite reloads it when we reload the page
      let module = await vite.moduleGraph.getModuleByUrl('/' + file);
      vite.moduleGraph.invalidateModule(module);
    }

    files.forEach(file => {
      let source = fs.readFileSync(file, 'utf8');
      if (gizmoShaderModifier.usesTransformMethod(source)) {
        let result;
        let updated = false;
        result = gizmoShaderModifier.addTransformMethodIfMissing(source);
        source = result.source;
        updated = updated || result.updated;
        result = gizmoShaderModifier.replaceTransformCallArguments(source, transformArgsGlsl);
        source = result.source;
        updated = updated || result.updated;
        if (updated) {
          writeFile(file, source);
        }
      }
    });

    res.statusCode = 200;
    res.end('Success');
  }
}