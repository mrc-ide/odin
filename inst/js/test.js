// code for use with a test model:
function run(name, pars, tStart, tEnd, nPoints, control) {
    // var odin = global[name];
    var solution = wodinRunner(dopri.Dopri, OdinBase, odin, pars,
                               tStart, tEnd, control);
    return solution(tStart, tEnd, nPoints);
}
