import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import { build } from 'vite'
import config from './vite.config.js';

const __dirname = path.dirname(url.fileURLToPath(import.meta.url))

const indexTemplate = fs.readFileSync(path.resolve(__dirname, 'index.html'), 'utf-8',);
const template = fs.readFileSync(path.resolve(__dirname, 'project.html'), 'utf-8',);

// determine routes to pre-render from src/pages
const projectsToRender = fs.readdirSync(
  path.resolve(__dirname, 'projects'),
  {withFileTypes: true}
)
  .filter(dirent => dirent.isDirectory())
  .map(dirent => dirent.name);

const tempHtmlDir = '.dist-html';
const tempHtmlDirAbs = path.resolve(__dirname, tempHtmlDir);

if (fs.existsSync(tempHtmlDirAbs)){
  fs.rmSync(tempHtmlDirAbs, { recursive: true })
}
fs.mkdirSync(tempHtmlDirAbs);

fs.mkdirSync(path.resolve(__dirname, '.dist-html/projects'));

let rollupInput = {};
let htmlPaths = [];

// pre-render each route...
for (const projectName of projectsToRender) {
  const html = template.replace(`<!--ssr-project-name-->`, projectName)
  const htmlPath = `projects/${projectName}/index.html`
  const filePath = `${tempHtmlDirAbs}/${htmlPath}`
  rollupInput[`projects/${projectName}`] = filePath;
  htmlPaths.push(htmlPath);
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  fs.writeFileSync(filePath, html)
  console.log('pre-rendered:', htmlPath)
}

// render index page
const projectsList = projectsToRender
  .map(name => `<li><a href="projects/${name}">${name}</a></li>\n`)
  .join('');
const html = indexTemplate.replace(`<!--ssr-projects-list-->`, projectsList)
const htmlPath = `index.html`
const filePath = `${tempHtmlDirAbs}/${htmlPath}`
rollupInput[`index`] = filePath;
htmlPaths.push(htmlPath);
fs.writeFileSync(filePath, html);

// build vite client
await build(Object.assign(config, {
  build: {
    rollupOptions: {
      input: rollupInput,
    },
  },
  experimental: {
    renderBuiltUrl(filename, { hostType }) {
      if (hostType == 'html') {
        return '../../' + filename;
      }
      return { relative: true };
    },
  },
}));

// move the generated html up a level
for (const htmlPath of htmlPaths) {
  const from = path.resolve(__dirname, 'dist', tempHtmlDir, htmlPath);
  const to = path.resolve(__dirname, 'dist', htmlPath);

  fs.mkdirSync(path.dirname(to), { recursive: true });
  fs.renameSync(from, to);
}

// remove temp directories
fs.rmSync(path.resolve(__dirname, 'dist', tempHtmlDir), { recursive: true })
fs.rmSync(path.resolve(__dirname, tempHtmlDir), { recursive: true })
