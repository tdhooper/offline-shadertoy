// https://vite.dev/guide/ssr

import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import express from 'express'
import https from 'https'
import { createServer as createViteServer } from 'vite'

const __dirname = path.dirname(fileURLToPath(import.meta.url))

async function createServer() {
  const app = express()

  const options = {
    key: fs.readFileSync('ssl/private-key.pem'),
    cert: fs.readFileSync('ssl/certificate.pem'),
  };
  const server = https.createServer(options, app)

  // Create Vite server in middleware mode and configure the app type as
  // 'custom', disabling Vite's own HTML serving logic so parent server
  // can take control
  const vite = await createViteServer({
    server: { middlewareMode: true },
    appType: 'custom'
  })

  // Use vite's connect instance as middleware. If you use your own
  // express router (express.Router()), you should use router.use
  // When the server restarts (for example after the user modifies
  // vite.config.js), `vite.middlewares` is still going to be the same
  // reference (with a new internal stack of Vite and plugin-injected
  // middlewares). The following is valid even after restarts.
  app.use(vite.middlewares)

  app.use('/projects/:project', async (req, res, next) => {
    const url = req.originalUrl
  
    try {
      let html = fs.readFileSync(
        path.resolve(__dirname, 'project.html'),
        'utf-8',
      )

      html = html.replace(`<!--ssr-project-name-->`, req.params.project)

      // Apply Vite HTML transforms. This injects the Vite HMR client,
      // and also applies HTML transforms from Vite plugins, e.g. global
      // preambles from @vitejs/plugin-react
      html = await vite.transformIndexHtml(url, html)

      // Send the rendered HTML back.
      res.status(200).set({ 'Content-Type': 'text/html' }).end(html)
    } catch (e) {
      // If an error is caught, let Vite fix the stack trace so it maps back
      // to your actual source code.
      vite.ssrFixStacktrace(e)
      next(e)
    }
  })

  app.get('/', async (req, res, next) => {
    const url = req.originalUrl
  
    try {
      let html = fs.readFileSync(
        path.resolve(__dirname, 'index.html'),
        'utf-8',
      )

      const projectsList = fs.readdirSync(
        path.resolve(__dirname, 'projects'),
        {withFileTypes: true}
      )
        .filter(dirent => dirent.isDirectory())
        .map(dirent => `<li><a href="/projects/${dirent.name}">${dirent.name}</a></li>\n`)
        .join('');

      html = html.replace(`<!--ssr-projects-list-->`, projectsList)

      // Apply Vite HTML transforms. This injects the Vite HMR client,
      // and also applies HTML transforms from Vite plugins, e.g. global
      // preambles from @vitejs/plugin-react
      html = await vite.transformIndexHtml(url, html)

      // Send the rendered HTML back.
      res.status(200).set({ 'Content-Type': 'text/html' }).end(html)
    } catch (e) {
      // If an error is caught, let Vite fix the stack trace so it maps back
      // to your actual source code.
      vite.ssrFixStacktrace(e)
      next(e)
    }
  })

  server.listen(5173)

  console.log("Listening on https://localhost:5173/");
}

createServer()