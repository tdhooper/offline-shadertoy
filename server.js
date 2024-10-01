const budo = require('budo');
const bodyParser = require('body-parser');
const empty = require('is-empty');
const Router = require('router');

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

  console.log(req.body);

  res.statusCode = 200;
  res.end('Success');
});
