// V8 does not support Array.fill from the look of it
function zeros(n) {
    var ret = new Array(n);
    for (var i = 0; i < n; ++i) {
        ret[i] = 0;
    }
    return ret;
}


function integrateOdin(obj, times, y0, control) {
    var t0 = times[0];
    var t1 = times[times.length - 1];
    if (obj.metadata.interpolateTimes !== null) {
        control.tcrit = interpolateCheckT(times, obj.metadata.interpolateTimes,
                                          control.tcrit);
    }
    if (isMissing(y0)) {
      y0 = obj.initial(times[0]);
    }
    var rhs = function(t, y, dy) {
        obj.rhs(t, y, dy);
    };
    var solver;
    if (typeof obj.output === "function") {
        var output = function(t, y) {
            return obj.output(t, y);
        }
        solver = new dopri.Dopri(rhs, y0.length, control, output);
    } else {
        solver = new dopri.Dopri(rhs, y0.length, control);
    }
    solver.initialise(t0, y0);
    var sol = solver.run(t1);
    var y = sol(times);
    // Prepend the result vector with the times; this is going to be
    // required later on - it would be nice if dopri did this through
    // it's interpolation function though.
    for (var i = 0; i < times.length; ++i) {
        y[i].unshift(times[i]);
    }

    return {"y": y,
            "names": obj.metadata.ynames.slice(0),
            "statistics": solver.statistics()};
}

function getUser(user, name, internal, size, defaultValue,
                 min, max, isInteger) {
    var value = user[name];
    if (isMissing(value)) {
        if (isMissing(internal[name])) {
            if (defaultValue === null) {
                throw Error("Expected a value for '" + name + "'");
            } else {
                internal[name] = defaultValue;
            }
        }
    } else {
        if (typeof value !== "number") {
            throw Error("Expected a numeric value for '" + name + "'");
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


function getUserArray(user, name, internal, size, defaultValue,
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

// With arrays there are really two ways that they might come in; on
// the one hand we could accept json-style nested arrays, which is
// cool, but requires considerable checking.  Or, given we want
// C-style arrays ultimately we could use an R-style construct like:
// {"data": <array>, "dim": <array>}.  I'm doing the latter for now,
// and we can revisit later.
function getUserArrayDim(user, name, internal, size, defaultValue,
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
    getUserArrayCheckContents(value.data, min, max, isInteger, name);

    var len = value.data.length;
    size[0] = len;
    for (var i = 0; i < rank; ++i) {
        size[i + 1] = value.dim[i];
    }

    internal[name] = value.data.slice();;
}


function getUserArrayCheckType(value, name) {
    if (Array.isArray(value)) {
        value = flattenArray(value, name)
    } else if (!(typeof value === "object" &&
                 "data" in value &&
                 "dim" in value)) {
        throw Error("Expected an odin.js array object for '" + name + "'");
    }
    return value;
}


function getUserArrayCheckRank(expected, given, name) {
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


function getUserArrayCheckContents(data, min, max, isInteger, name) {
    for (var i = 0; i < data.length; ++i) {
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


function checkUser(user, allowed, unusedUserAction) {
    if (unusedUserAction === undefined) {
        unusedUserAction = "stop";
    }
    if (unusedUserAction === "ignore") {
        return;
    }
    var err = setDifference(Object.keys(user), allowed);
    if (err.length > 0) {
        var msg = "Unknown user parameters: " + err.join(", ");

        if (unusedUserAction === "message") {
            odinMessage(msg);
        } else if (unusedUserAction === "warning") {
            odinWarning(msg);
        } else if (unusedUserAction === "stop") {
            throw Error(msg);
        } else {
            throw Error(msg + " (and invalid value for unusedUserAction)");
        }
    }
}


function isMissing(x) {
    return x === undefined || x === null ||
        (typeof x === "number" && isNaN(x));
}


// Travis has ancient v8 version that lacks Number.isNumber.  However
// it also lacks Number.EPSILON so I'm just comparing against 1e-8
// which is close enough to sqrt(double.eps) anyway
function numberIsInteger(x) {
    return Math.abs(x - Math.round(x)) < 1e-8
}


// O(n^2) but does not use Set, which is not available in old v8
function setDifference(a, b) {
  var result = [];
  for (var i = 0; i < a.length; i++) {
    if (b.indexOf(a[i]) === -1) {
      result.push(a[i]);
    }
  }
  return result;
}


// nice behaviour both in and out of R
function odinWarning(msg) {
    try {
        console.r.call("warning", msg)
    } catch (e) {
        console.warn(msg)
    }
}


function odinMessage(msg) {
    try {
        console.r.call("message", msg)
    } catch (e) {
        console.warn(msg)
    }
}


function flattenArray(value, name) {
    var len = 1;
    var dim = [];
    var x = value;
    while (Array.isArray(x)) {
        dim.push(x.length);
        len *= x.length;
        x = x[0];
    }
    dim.reverse();

    var data = flatten(value, []);

    // Not a suffient check, but at least a necessary one:
    if (len !== data.length) {
        throw Error("Inconsistent array for '" + name + '"');
    }

    return {data: data, dim: dim};
}


function flatten(array, result) {
  if (array.length === 0) {
    return result
  }
  var head = array[0]
  var rest = array.slice(1)
  if (Array.isArray(head)) {
    return flatten(head.concat(rest), result)
  }
  result.push(head)
  return flatten(rest, result)
}


// https://en.wikipedia.org/wiki/Rounding#Round_half_to_even - same
// behaviour as R and IEEE 754, with better biases.
function roundHalfToEven(x) {
    if (modr(x, 1) === 0.5) {
        return 2 * Math.round(x / 2);
    } else {
        return Math.round(x);
    }
}


function round2(x, digits) {
    if (digits === undefined) {
        return roundHalfToEven(x);
    } else {
        var mult = Math.pow(10, digits);
        return roundHalfToEven(x * mult) / mult;
    }
}

// modulo that conforms to (approximately) the same behaviour as R
function modr(x, y) {
    var tmp = x % y;
    if (tmp * y < 0) {
        tmp += y;
    }
    return tmp;
}


function intdivr(x, y) {
    return Math.floor(x / y);
}


// Ranks 2..8 created by scripts/create_support_sum_js.R
function odinSum1(x, from, to) {
    var tot = 0.0;
    for (var i = from; i < to; ++i) {
        tot += x[i];
    }
    return tot;
}
