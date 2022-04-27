// Wrapper for R
class OdinWrapper {
    constructor(Base, Model, pars, unusedUserAction) {
        this.model = new Model(Base, pars, unusedUserAction);
    }

    initial(t) {
        return this.model.initial(t);
    }

    setUser(user, unusedUserAction) {
        this.model.setUser(pars, unusedUserAction);
    }

    rhs(t, y) {
        var state = OdinBase.zeros(y.length);
        var output = OdinBase.zeros(0); // good enough really.
        this.model.rhs(t, y, state, output);
        return {state: state, output: output};
    }

    getMetadata() {
        return this.model.metadata;
    }

    getInternal() {
        return this.model.internal;
    }

    run(t, y0, control) {
        const tStart = t[0];
        const tEnd = t[t.length - 1];
        const result = this.model.run(tStart, tEnd, y0, control, dopri.Dopri);
        return {y: result.solution(t),
                names: result.names,
                statistics: result.statistics};
    }
}
