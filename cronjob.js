const cron = require('node-cron');
var spawn = require('child_process').spawn
 
cron.schedule('30 23 * * *', () => {
    twitter = spawn('./twitter.R')
    twitter.stdout.on('data', function (data) {
      console.log('stdout: ' + data.toString());
    });
    
    twitter.stderr.on('data', function (data) {
      console.log(data.toString());
    });
    
    twitter.on('exit', function (code) {
      console.log('child process exited with code ' + code.toString());
    });
});



