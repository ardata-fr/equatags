var mjAPI = require("mathjax-node");
const myArgs = process.argv.slice(2);
mjAPI.start();

myArgs.forEach(function(element){
  mjAPI.typeset({
    math: element,
    mml: true
  }, function(result) {
    console.log(result.mml);
  });
});
