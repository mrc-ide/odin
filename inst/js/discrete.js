function iterateOdin(obj, steps, y, nOut) {
    if (obj.metadata.interpolateTimes !== null) {
        interpolateCheckT(steps, obj.metadata.interpolateTimes, null);
    }
    if (isMissing(y)) {
        y = obj.initial(steps[0]);
    }
    var update = function(step, y, yNext, output) {
        obj.rhs(step, y, yNext, output);
    }

    return {"y": difeq(update, steps, y, nOut),
            "names": obj.metadata.ynames.slice(0)};
}


function difeq(update, steps, y, nOut) {
    var step = steps[0], stepEnd = steps[steps.length - 1];

    var yNext = new Array(y.length);
    var output = new Array(nOut);
    var hasOutput = nOut > 0;
    var storeNextOutput = hasOutput;

    var ret = [];
    var last = [step].concat(y);
    ret.push(last);
    var idx = 1;

    while (true) {
        update(step, y, yNext, output);
        step++;
        y = yNext;

        if (storeNextOutput) {
            for (var i = 0; i < nOut; ++i) {
                last.push(output[i]);
            }
            storeNextOutput = false;
        }

        if (step == steps[idx]) {
            last = [step].concat(y);
            ret.push(last);
            storeNextOutput = hasOutput;
            idx++;
        }

        if (step == stepEnd) {
            break;
        }
    }

    if (storeNextOutput) {
        update(step, y, yNext, output);
        for (var i = 0; i < nOut; ++i) {
            last.push(output[i]);
        }
    }

    return ret;
}
