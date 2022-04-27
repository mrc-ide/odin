"use strict";

class OdinRunner {
    constructor(Dopri) {
        this.Dopri = Dopri;
    }

    // Wrapper for wodin only
    runModel(model, tStart, tEnd, y0, control) {
        const grid = function(from, to, len) {
            const dx = (to - from) / (len - 1);
            const x = [];
            for (let i = 0; i < len; ++i) {
                x.push(from + i * dx);
            }
            return x;
        }

        const result = model.run(tStart, tEnd, control, this.Dopri);
        const solution = result.solution;
        const names = result.names;
        return function(t0, t1, nPoints) {
            const t = grid(Math.max(0, t0), Math.min(t1, tEnd), nPoints);
            const y = solution(t);
            return y[0].map((_, i) => ({
                x: t, y: y.map(row => row[i]), name: names[i]}));
        }
   }
}

class OdinBase {
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
                odinMessage(msg); // does not exist?
            } else if (unusedUserAction === "warning") {
                odinWarning(msg); // does not exist?
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
            if (max !== null && value > min) {
                throw Error("Expected '" + name + "' to be at most " + max);
            }
            if (isInteger && !numberIsInteger(value)) {
                throw Error("Expected '" + name + "' to be integer-like");
            }

            internal[name] = value;
        }
    }

    static getUserArray(user, name, internal, size, defaultValue,
                        min, max, isInteger) {
        var value = user[name];

        if (isMissing(value)) {
            if (isMissing(internal[name])) {
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
        value = getUserArrayCheckType(value, name);
        getUserArrayCheckRank(rank, value.dim.length, name);

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

        getUserArrayCheckContents(value.data, min, max, isInteger, name);

        internal[name] = value.data.slice();
    }

    static getUserArrayCheckType(value, name) {
        if (Array.isArray(value)) {
            value = flattenArray(value, name)
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
            if (isInteger && !numberIsInteger(data[i])) {
                throw Error("Expected a integer value for '" + name + "'");
            }
        }
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

    static run(tStart, tEnd, y0, control, model, Dopri) {
        if (y0 === null) {
            y0 = model.initial(tStart);
        }
        const rhs = function(t, y, dydt) {
            model.rhs(t, y, dydt);
        }
        // const output = function(t, y) {
        //     return obj.output(t, y);
        // }
        // const solver = typeof obj.output === "function" ? undefined

        // var solver;
        // if (typeof obj.output === "function") {
        //     var output = function(t, y) {
        //         return obj.output(t, y);
        //     }
        //     solver = new dopri.Dopri(rhs, y0.length, control, output);
        // } else {
        //     solver = new dopri.Dopri(rhs, y0.length, control);
        // }

        // const output = model.outputOrder === null ? null :
        //       (t, y) => model.output(t, y);
        const output = null;
        const solver = new Dopri(rhs, y0.length, control, output);
        solver.initialise(tStart, y0);
        const solution = solver.run(tEnd);
        const names = model.metadata.ynames.slice(1);
        const statistics = solver.statistics();
        return {solution: solution, names: names, statistics: statistics};
    }
}
