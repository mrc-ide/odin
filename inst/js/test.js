function call_odin_bundle(Model, pars, tStart, tEnd, nPoints, control) {
    const solution = odinjs.wodinRun(Model, pars, tStart, tEnd, control);
    return solution(tStart, tEnd, nPoints);
}
