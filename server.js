var budo = require('budo');
var Router = require('router');
var browserify = require('browserify');


var browserifyOpts = {
  plugin: [
    require('regl')
  ]
};

var router = Router();

budo('index.js', {
  live: true,
  stream: process.stdout,
  ssl: true,
  css: '/styles/main.css',
  middleware: [
    router
  ],
  browserify: browserifyOpts
});

var renderWorker = new Promise((resolve, reject) => {
  browserify('render.js', browserifyOpts).bundle((err, buffer) => {
    if (err) {
      reject(err);
    } else {
      resolve(buffer);
    }
  });
});

router.get('/render.js', function(req, res) {
  renderWorker.then(
    buffer => {
      res.statusCode = 200;
      res.end(buffer);
    },
    err => {
      res.statusCode = 500;
      res.end(err);
    }
  );
});
