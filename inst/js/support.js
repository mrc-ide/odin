"use strict";

function wodinRunner(dopri, Model, pars, tStart, tEnd, control) {
    const grid = function(from, to, len) {
        const dx = (to - from) / (len - 1);
        const x = [];
        for (let i = 0; i < len; ++i) {
            x.push(from + i * dx);
        }
        return x;
    }

    const model = new Model(OdinBase, pars, "error");
    const y0 = null; // Always use model-provide initial conditions
    const result = model.run(tStart, tEnd, y0, control, dopri);
    const solution = result.solution;
    const names = result.names;
    return function(t0, t1, nPoints) {
        const t = grid(Math.max(0, t0), Math.min(t1, tEnd), nPoints);
        const y = solution(t);
        return y[0].map((_, i) => ({
            x: t, y: y.map(row => row[i]), name: names[i]}));
    }
}

class OdinBase {
    static delay(solution, t, index, state) {
        // Later, we'll update dopri.js to allow passing index here,
        // which will make this more efficient. However, no change to
        // the external interface will be neeed.
        var y = solution(t);
        for (var i = 0; i < index.length; ++i) {
            state[i] = y[index[i]];
        }
    }

    static isMissing(x) {
        return x === undefined || x === null ||
            (typeof x === "number" && isNaN(x));
    }

    static setDifference(a, b) {
        const result = [];
        for (let i = 0; i < a.length; i++) {
          if (b.indexOf(a[i]) === -1) {
            result.push(a[i]);
          }
        }
        return result;
    }

    static checkUser(user, allowed, unusedUserAction) {
        if (unusedUserAction === undefined) {
            unusedUserAction = "stop";
        }
        if (unusedUserAction === "ignore") {
            return;
        }
        const err = OdinBase.setDifference(Object.keys(user), allowed);
        if (err.length > 0) {
            const msg = "Unknown user parameters: " + err.join(", ");

            if (unusedUserAction === "message") {
                OdinBase.odinMessage(msg);
            } else if (unusedUserAction === "warning") {
                OdinBase.odinWarning(msg);
            } else if (unusedUserAction === "stop") {
                throw Error(msg);
            } else {
                throw Error(msg + " (and invalid value for unusedUserAction)");
            }
        }
    }

    static getUser(user, name, internal, size, defaultValue,
                   min, max, isInteger) {
        const value = user[name];
        if (OdinBase.isMissing(value)) {
            if (OdinBase.isMissing(internal[name])) {
                if (defaultValue === null) {
                    throw Error("Expected a value for '" + name + "'");
                } else {
                    internal[name] = defaultValue;
                }
            }
        } else {
            if (typeof value !== "number") {
                if (typeof value === "string") {
                    throw Error("Expected a numeric value for '" + name + "'");
                } else {
                    throw Error("Expected a scalar numeric for '" + name + "'");
                }
            }
            if (min !== null && value < min) {
                throw Error("Expected '" + name + "' to be at least " + min);
            }
            if (max !== null && value > max) {
                throw Error("Expected '" + name + "' to be at most " + max);
            }
            if (isInteger && !OdinBase.numberIsInteger(value)) {
                throw Error("Expected '" + name + "' to be integer-like");
            }

            internal[name] = value;
        }
    }

    static getUserArray(user, name, internal, size, defaultValue,
                        min, max, isInteger) {
        var value = user[name];

        if (OdinBase.isMissing(value)) {
            if (OdinBase.isMissing(internal[name])) {
                if (defaultValue === null) {
                    throw Error("Expected a value for '" + name + "'");
                } else {
                    // not totally clear how to do this as we need to get
                    // the previous dimensions too!
                    throw Error("This needs implementing....");
                    internal[name] = defaultValue;
                }
            }
            return;
        }

        var rank = size.length - 1;
        value = OdinBase.getUserArrayCheckType(value, name);
        OdinBase.getUserArrayCheckRank(rank, value.dim.length, name);

        for (var i = 0; i < rank; ++i) {
            if (value.dim[i] !== size[i + 1]) {
                if (rank == 1) {
                    throw Error("Expected length " + size[i + 1] +
                                " value for '" + name + "'");
                } else {
                    throw Error("Incorrect size of dimension " + (i + 1) +
                                " of '" + name + "' (expected " + size[i + 1] +
                                ")");
                }
            }
        }

        OdinBase.getUserArrayCheckContents(value.data, min, max, isInteger, name);

        internal[name] = value.data.slice();
    }

    static getUserArrayDim(user, name, internal, size, defaultValue,
                           min, max, isInteger) {
        var value = user[name];

        if (OdinBase.isMissing(value)) {
            if (OdinBase.isMissing(internal[name])) {
                if (defaultValue === null) {
                    throw Error("Expected a value for '" + name + "'");
                } else {
                    // not totally clear how to do this as we need to get
                    // the previous dimensions too!
                    throw Error("This needs implementing....");
                    internal[name] = defaultValue;
                }
            }
            return;
        }

        var rank = size.length - 1;
        value = OdinBase.getUserArrayCheckType(value, name);
        OdinBase.getUserArrayCheckRank(rank, value.dim.length, name);
        OdinBase.getUserArrayCheckContents(value.data, min, max, isInteger, name);

        var len = value.data.length;
        size[0] = len;
        for (var i = 0; i < rank; ++i) {
            size[i + 1] = value.dim[i];
        }

        internal[name] = value.data.slice();;
    }

    static getUserArrayCheckType(value, name) {
        if (Array.isArray(value)) {
            value = OdinBase.flattenArray(value, name)
        } else if (typeof value === "number") {
            // promote scalar number to vector, in the hope that's close
            // enough to what the user wants; this does give some
            // reasonable error messages relative to the C version.
            value = {data: [value], dim: [1]}
        } else if (!(typeof value === "object" &&
                     "data" in value &&
                     "dim" in value)) {
            throw Error("Expected an odin.js array object for '" + name + "'");
        }
        return value;
    }

    static getUserArrayCheckRank(expected, given, name) {
        if (given !== expected) {
            if (expected === 1) {
                throw Error("Expected a numeric vector for '" + name + "'");
            } else if (expected === 2) {
                throw Error("Expected a numeric matrix for '" + name + "'");
            } else {
                throw Error("Expected a numeric array of rank " + expected +
                            " for '" + name + "'");
            }
        }
    }

    static getUserArrayCheckContents(data, min, max, isInteger, name) {
        for (var i = 0; i < data.length; ++i) {
            if (data[i] === null) {
                throw Error("'" + name + "' must not contain any NA values");
            }
            if (typeof data[i] !== "number") {
                throw Error("Expected a numeric value for '" + name + "'");
            }
            if (min !== null && data[i] < min) {
                throw Error("Expected '" + name + "' to be at least " + min);
            }
            if (max !== null && data[i] > min) {
                throw Error("Expected '" + name + "' to be at most " + max);
            }
            if (isInteger && !OdinBase.numberIsInteger(data[i])) {
                throw Error("Expected a integer value for '" + name + "'");
            }
        }
    }

    static numberIsInteger(x) {
        return Math.abs(x - Math.round(x)) < 1e-8;
    }

    static zeros(n) {
        return Array(n).fill(0);
    }

    static grid(from, to, len) {
        const dx = (to - from) / (len - 1);
        const x = [];
        for (let i = 0; i < len; ++i) {
            x.push(from + i * dx);
        }
        return x;
    }

    static flattenArray(value, name) {
        var len = 1;
        var dim = [];
        var x = value;
        while (Array.isArray(x)) {
            dim.push(x.length);
            len *= x.length;
            x = x[0];
        }
        dim.reverse();

        var data = OdinBase.flatten(value, []);

        // Not a suffient check, but at least a necessary one:
        if (len !== data.length) {
            throw Error("Inconsistent array for '" + name + '"');
        }

        return {data: data, dim: dim};
    }

    static flatten(array, result) {
        if (array.length === 0) {
            return result
        }
        var head = array[0]
        var rest = array.slice(1)
        if (Array.isArray(head)) {
            return OdinBase.flatten(head.concat(rest), result)
        }
        result.push(head)
        return OdinBase.flatten(rest, result)
    }

    // NOTE: dopri here, not Dopri - breaking wodin change
    static run(tStart, tEnd, y0, control, model, dopri) {
        if (y0 === null) {
            y0 = model.initial(tStart);
        }
        // TODO: All this logic could be easily put into a function or
        // methods within the dopri package. Something like
        // `createSolver` seems sensible?
        const hasDelay = model.rhs.length === 4;
        const hasOutput = typeof model.output === "function";
        let rhs;
        let output = null;
        let Solver;
        if (hasDelay) {
            Solver = dopri.DDE;
            rhs = function(t, y, dydt, solution) {
                model.rhs(t, y, dydt, solution);
            }
            if (hasOutput) {
                output = ((t, y, solution) => model.output(t, y, solution));
            }
        } else {
            Solver = dopri.Dopri;
            rhs = function(t, y, dydt) {
                model.rhs(t, y, dydt);
            }
            if (hasOutput) {
                output = ((t, y) => model.output(t, y));
            }
        }
        const solver = new Solver(rhs, y0.length, control, output);
        solver.initialise(tStart, y0);
        const solution = solver.run(tEnd);
        const names = model.metadata.ynames.slice(1);
        const statistics = solver.statistics();
        return {solution: solution, names: names, statistics: statistics};
    }

    static odinWarning(msg) {
        try {
            console.r.call("warning", msg)
        } catch (e) {
            console.warn(msg)
        }
    }

    static odinMessage(msg) {
        try {
            console.r.call("message", msg)
        } catch (e) {
            console.warn(msg)
        }
    }

    static roundHalfToEven(x) {
        if (modr(x, 1) === 0.5) {
            return 2 * Math.round(x / 2);
        } else {
            return Math.round(x);
        }
    }


    static round2(x, digits) {
        if (digits === undefined) {
            return roundHalfToEven(x);
        } else {
            var mult = Math.pow(10, digits);
            return roundHalfToEven(x * mult) / mult;
        }
    }

    // modulo that conforms to (approximately) the same behaviour as R
    static modr(x, y) {
        var tmp = x % y;
        if (tmp * y < 0) {
            tmp += y;
        }
        return tmp;
    }


    static intdivr(x, y) {
        return Math.floor(x / y);
    }

    static odinSum1(x, from, to) {
        var tot = 0.0;
        for (var i = from; i < to; ++i) {
            tot += x[i];
        }
        return tot;
    }

    static odinSum2(x, iFrom, iTo, jFrom, jTo, dim1) {
        var tot = 0.0;
        for (var j = jFrom; j < jTo; ++j) {
            var jj = j * dim1;
            for (var i = iFrom; i < iTo; ++i) {
                tot += x[i + jj];
            }
        }
        return tot;
    }

    static odinSum3(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, dim1, dim12) {
        var tot = 0.0;
        for (var k = kFrom; k < kTo; ++k) {
            var kk = k * dim12;
            for (var j = jFrom; j < jTo; ++j) {
                var jj = j * dim1 + kk;
                for (var i = iFrom; i < iTo; ++i) {
                    tot += x[i + jj];
                }
            }
        }
        return tot;
    }

    static odinSum4(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, dim1, dim12, dim123) {
        var tot = 0.0;
        for (var l = lFrom; l < lTo; ++l) {
            var ll = l * dim123;
            for (var k = kFrom; k < kTo; ++k) {
                var kk = k * dim12 + ll;
                for (var j = jFrom; j < jTo; ++j) {
                    var jj = j * dim1 + kk;
                    for (var i = iFrom; i < iTo; ++i) {
                        tot += x[i + jj];
                    }
                }
            }
        }
        return tot;
    }

    static odinSum5(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, dim1, dim12, dim123, dim1234) {
        var tot = 0.0;
        for (var i5 = i5From; i5 < i5To; ++i5) {
            var i5i5 = i5 * dim1234;
            for (var l = lFrom; l < lTo; ++l) {
                var ll = l * dim123 + i5i5;
                for (var k = kFrom; k < kTo; ++k) {
                    var kk = k * dim12 + ll;
                    for (var j = jFrom; j < jTo; ++j) {
                        var jj = j * dim1 + kk;
                        for (var i = iFrom; i < iTo; ++i) {
                            tot += x[i + jj];
                        }
                    }
                }
            }
        }
        return tot;
    }

    static odinSum6(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, i6From, i6To, dim1, dim12, dim123, dim1234, dim12345) {
        var tot = 0.0;
        for (var i6 = i6From; i6 < i6To; ++i6) {
            var i6i6 = i6 * dim12345;
            for (var i5 = i5From; i5 < i5To; ++i5) {
                var i5i5 = i5 * dim1234 + i6i6;
                for (var l = lFrom; l < lTo; ++l) {
                    var ll = l * dim123 + i5i5;
                    for (var k = kFrom; k < kTo; ++k) {
                        var kk = k * dim12 + ll;
                        for (var j = jFrom; j < jTo; ++j) {
                            var jj = j * dim1 + kk;
                            for (var i = iFrom; i < iTo; ++i) {
                                tot += x[i + jj];
                            }
                        }
                    }
                }
            }
        }
        return tot;
    }

    static odinSum7(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, i6From, i6To, i7From, i7To, dim1, dim12, dim123, dim1234, dim12345, dim123456) {
        var tot = 0.0;
        for (var i7 = i7From; i7 < i7To; ++i7) {
            var i7i7 = i7 * dim123456;
            for (var i6 = i6From; i6 < i6To; ++i6) {
                var i6i6 = i6 * dim12345 + i7i7;
                for (var i5 = i5From; i5 < i5To; ++i5) {
                    var i5i5 = i5 * dim1234 + i6i6;
                    for (var l = lFrom; l < lTo; ++l) {
                        var ll = l * dim123 + i5i5;
                        for (var k = kFrom; k < kTo; ++k) {
                            var kk = k * dim12 + ll;
                            for (var j = jFrom; j < jTo; ++j) {
                                var jj = j * dim1 + kk;
                                for (var i = iFrom; i < iTo; ++i) {
                                    tot += x[i + jj];
                                }
                            }
                        }
                    }
                }
            }
        }
        return tot;
    }

    static odinSum8(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, i6From, i6To, i7From, i7To, i8From, i8To, dim1, dim12, dim123, dim1234, dim12345, dim123456, dim1234567) {
        var tot = 0.0;
        for (var i8 = i8From; i8 < i8To; ++i8) {
            var i8i8 = i8 * dim1234567;
            for (var i7 = i7From; i7 < i7To; ++i7) {
                var i7i7 = i7 * dim123456 + i8i8;
                for (var i6 = i6From; i6 < i6To; ++i6) {
                    var i6i6 = i6 * dim12345 + i7i7;
                    for (var i5 = i5From; i5 < i5To; ++i5) {
                        var i5i5 = i5 * dim1234 + i6i6;
                        for (var l = lFrom; l < lTo; ++l) {
                            var ll = l * dim123 + i5i5;
                            for (var k = kFrom; k < kTo; ++k) {
                                var kk = k * dim12 + ll;
                                for (var j = jFrom; j < jTo; ++j) {
                                    var jj = j * dim1 + kk;
                                    for (var i = iFrom; i < iTo; ++i) {
                                        tot += x[i + jj];
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return tot;
    }
}
