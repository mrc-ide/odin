global.seedrandom = require("seedrandom");
global.random = require("random");
global.random.use(seedrandom("odin.js"));
global.setSeed = function(seed) {
    global.random.use(seedrandom(seed));
}
// Configure sensible drop-in replacements for R's functions:
global.random.unifRand = random.uniform();
global.random.normRand = random.normal();
global.random.expRand = random.exponential();

// Patched to reflect R's behaviour and my needs:
global.random.rbinom = function(n, p) {
    n = Math.round(n);
    var ret = null;
    if (n === 0) {
        ret = function() {
            return 0;
        }
    } else {
        ret = global.random.binomial(n, p);
    }
    return ret;
}
