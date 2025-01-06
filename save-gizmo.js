import empty from 'is-empty';
import fs from 'node:fs';
import path from 'path';
import gizmoShaderModifier from './lib/gizmo/gizmo-shader-modifier.js'

export default function saveGizmo(vite) {
  
  
  const roundVec = (vec, dp) => {
    let r = Math.pow(10, dp);
    return vec.map(v => { return Math.round(v * r) / r });
  }

  const writeFile = async (file, content) => {
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

  const adjustmentArgsAsGlsl = (gizmoAdjustment) => {
    return ([
      `vec3(${roundVec(gizmoAdjustment.t, 7).join(',')})`,
      `vec4(${roundVec(gizmoAdjustment.r, 7).join(',')})`,
      `vec3(${roundVec(gizmoAdjustment.s, 7).join(',')})`,
    ]).join(', ');
  }

  const adjustFile = async (file, gizmoAdjustments) => {

    const projectsPath = path.parse(path.normalize('./projects/'));

    let parsed = path.parse(path.normalize(file));
    if (parsed.dir.indexOf(projectsPath.dir) !== 0) {
      console.log(`${file} directory ${parsed.dir} outside of projects directory ${projectsPath.dir}`);
      return;
    }

    const transformArgsGlsl = gizmoAdjustments.map(gizmoAdjustment => adjustmentArgsAsGlsl(gizmoAdjustment));

    let source = fs.readFileSync(file, 'utf8');

    if ( ! gizmoShaderModifier.usesTransformMethod(source)) {
      return;
    }

    let result;
    let updated = false;

    result = gizmoShaderModifier.addTransformMethodIfMissing(source);
    source = result.source;
    updated = updated || result.updated;

    result = gizmoShaderModifier.replaceTransformCallArguments(source, transformArgsGlsl);
    source = result.source;
    updated = updated || result.updated;

    if (updated) {
      await writeFile(file, source);
    }
  }

  return async function post(req, res, next) {

    res.set({ 'Content-Type': 'text/html' });

    if (empty(req.body)) {
      res.statusCode = 500;
      res.end('Missing content');
      return;
    }

    if (empty(req.body.file)) {
      res.statusCode = 500;
      res.end('No file specified');
      return;
    }

    if (empty(req.body.gizmoAdjustments)) {
      res.statusCode = 500;
      res.end('No adjustments specified');
      return;
    }

    await adjustFile(req.body.file, req.body.gizmoAdjustments);

    res.statusCode = 200;
    res.end('Success');
  }
}