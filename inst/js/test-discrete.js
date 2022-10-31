function call_odin_bundle(Model, pars, tStart, tEnd, dt, nParticles) {
    const solution = dust.wodinRunDiscrete(Model, pars, tStart, tEnd, dt, nParticles);
    return solution({mode: "grid", tStart, tEnd, nPoints: Infinity});
}
