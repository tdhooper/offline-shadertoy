var budo = require('budo');
var Router = require('router');
var browserify = require('browserify');

var router = Router();

var renderWorker;

var bundleWorker = () => {
  console.log("Bundling worker");
  renderWorker = new Promise((resolve, reject) => {
    browserify('render.js').bundle((err, buffer) => {
      if (err) {
        reject(err);
      } else {
        resolve(buffer);
      }
    });
  });
};

bundleWorker();

budo('index.js', {
  live: true,
  stream: process.stdout,
  ssl: true,
  css: '/styles/main.css',
  middleware: [
    router
  ]
}).on('reload', function() {
  bundleWorker();
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
