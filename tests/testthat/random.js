// A little helper to generate random numbers from the dust rng from
// any distribution; we need this to make the tests easy!
//
// Note that this reuses inst/js/dust-rng.js for calling back to R so
// that the random number generation respects set.seed (this is all a
// bit nasty, and we'll probably sort this out more elegantly in
// dust.js later, but there's some fairly unresolvable issues with
// seedable random number streams in js to deal with first).
//
// We do the serialisation to JSON string here rather than relying on
// V8 as this preserves full precision.
function random(distribution, n, args) {
    const rng = {
        random: function() {
            return JSON.parse(console.r.call("function() jsonlite::toJSON(runif(1), digits = NA, auto_unbox = TRUE)", []));
        }
    };
    let r = new dust.PkgWrapper.random(rng);
    const ret = [];
    for (let i = 0; i < n; ++i) {
        ret.push(r[distribution](...args));
    }
    return JSON.stringify(ret);
}
