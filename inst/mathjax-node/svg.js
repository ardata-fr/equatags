var mjAPI = require("mathjax-node");
const myArgs = process.argv.slice(2);
mjAPI.start();

myArgs.forEach(function(element){
  mjAPI.typeset({
    math: element,
    svg: true
  }, function(result) {
    console.log(result.svg);
  });
});
