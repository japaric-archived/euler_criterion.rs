function solution() {
    var sum = 0;
    var i;

    for (i = 0; i < 1000; i++) {
        if (i % 3 === 0 || i % 5 === 0) {
            sum += i;
        }
    }

    return sum;
}

if (process.argv.length === 3 && process.argv[2] === "-a") {
    console.log(solution());
    process.exit();
}

var clock = require("posix-clock");
var lazy = require("lazy");

new lazy(process.stdin).
    lines.
    forEach(function(line) {
        var iters = parseInt(line, 10);

        var start = clock.gettime(clock.MONOTONIC);
        var i;
        for (i = 0; i < iters; i++) {
            solution();
        }
        var end = clock.gettime(clock.MONOTONIC);

        var secs = end.sec - start.sec;
        var nsecs = end.nsec - start.nsec;

        console.log(secs * 1000000000 + nsecs);
    });
