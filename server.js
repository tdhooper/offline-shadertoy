const budo = require('budo');

budo('./index.js', {
  live: true,
  stream: process.stdout,
  ssl: true,
  css: '/styles/main.css',
});
