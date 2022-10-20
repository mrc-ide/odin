// A small js rng that we can seed via R. Some faff is required here
// because we need to serialise the returned values carefully to avoid
// jsonlite truncating them to a few significant figures
{
    random: function() {
        return JSON.parse(console.r.call("function() jsonlite::toJSON(runif(1), digits = NA, auto_unbox = TRUE)", []));
    }
}
