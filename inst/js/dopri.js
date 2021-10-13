(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
(function (global){(function (){
global.dopri = require('dopri');

}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"dopri":8}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function dopriControl(control) {
    if (control === void 0) { control = {}; }
    var defaults = { atol: 1e-6,
        maxSteps: 10000,
        rtol: 1e-6,
        stepSizeMax: Infinity,
        stepSizeMin: 1e-8,
        stepSizeMinAllow: false,
        stiffCheck: 0,
        tcrit: Infinity,
    };
    var ret = {
        atol: withDefault(control.atol, defaults.atol),
        maxSteps: withDefault(control.maxSteps, defaults.maxSteps),
        rtol: withDefault(control.rtol, defaults.rtol),
        stepSizeMax: withDefault(control.stepSizeMax, defaults.stepSizeMax),
        stepSizeMin: withDefault(control.stepSizeMin, defaults.stepSizeMin),
        stepSizeMinAllow: withDefault(control.stepSizeMinAllow, defaults.stepSizeMinAllow),
        stiffCheck: withDefault(control.stiffCheck, defaults.stiffCheck),
        tcrit: withDefault(control.tcrit, defaults.tcrit),
    };
    if (ret.maxSteps < 1) {
        throw controlError("maxSteps", "must be at least 1");
    }
    if (ret.atol <= 0) {
        throw controlError("atol", "must be strictly positive");
    }
    if (ret.rtol <= 0) {
        throw controlError("rtol", "must be strictly positive");
    }
    return ret;
}
exports.dopriControl = dopriControl;
function controlError(nm, message) {
    return new Error("Invalid control parameter: '" + nm + "' " + message);
}
function withDefault(x, y) {
    return x === undefined ? y : x;
}

},{}],3:[function(require,module,exports){
"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
var dopri_1 = require("./dopri");
var utils_1 = require("./utils");
function integrate(rhs, y, t0, t1, control, output) {
    if (control === void 0) { control = {}; }
    if (output === void 0) { output = null; }
    var solver = new DDE(rhs, y.length, control, output);
    solver.initialise(t0, y);
    return solver.run(t1);
}
exports.integrate = integrate;
var DDE = /** @class */ (function (_super) {
    __extends(DDE, _super);
    function DDE(rhs, n, control, output) {
        if (control === void 0) { control = {}; }
        if (output === void 0) { output = null; }
        var _this = this;
        var solution = function (t) { return _this._interpolate(t); };
        var rhsUse = function (t, y, dy) {
            return rhs(t, y, dy, solution);
        };
        var outputUse = output === null ? null :
            function (t, y) { return output(t, y, solution); };
        _this = _super.call(this, rhsUse, n, control, outputUse) || this;
        _this._y0 = new Array(n);
        return _this;
    }
    DDE.prototype.initialise = function (t, y) {
        _super.prototype.initialise.call(this, t, y);
        this._y0 = y;
        return this;
    };
    DDE.prototype._interpolate = function (t) {
        var i = this._findHistory(t);
        if (i < 0) {
            return this._y0.slice(0);
        }
        else {
            return this._stepper.interpolate(t, this._history[i]);
        }
    };
    DDE.prototype._findHistory = function (t) {
        return utils_1.search(this._history, function (el) { return el.t > t; });
    };
    return DDE;
}(dopri_1.Dopri));
exports.DDE = DDE;

},{"./dopri":4,"./utils":12}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var control_1 = require("./control");
var dopri5 = require("./dopri5/stepper");
var interpolator_1 = require("./interpolator");
var utils = require("./utils");
// needed for ES5 - will be ~= Number.EPSILON in ES6
var DBL_EPSILON = Math.pow(2, -52); // = 2.220446049250313e-16
var STEP_FACTOR_MIN = 1e-4;
function integrate(rhs, y, t0, t1, control, output) {
    if (control === void 0) { control = {}; }
    if (output === void 0) { output = null; }
    var solver = new Dopri(rhs, y.length, control, output);
    solver.initialise(t0, y);
    return solver.run(t1);
}
exports.integrate = integrate;
function integrationError(message, t) {
    return new Error("Integration failure: " + message + " at " + t);
}
var Dopri = /** @class */ (function () {
    function Dopri(rhs, n, control, output) {
        if (control === void 0) { control = {}; }
        if (output === void 0) { output = null; }
        this._history = [];
        this._t = 0.0;
        this._h = 0.0;
        // state
        this._nSteps = 0;
        this._nStepsAccepted = 0;
        this._nStepsRejected = 0;
        this._stiffNStiff = 0;
        this._stiffNNonstiff = 0;
        this._lastError = 0;
        this._stepper = new dopri5.Dopri5(rhs, n);
        this._control = control_1.dopriControl(control);
        this._output = output;
    }
    Dopri.prototype.initialise = function (t, y) {
        var n = this._stepper.n;
        if (y.length !== n) {
            throw Error("Invalid size 'y' - expected a length " + n + " array");
        }
        this._stepper.reset(t, y);
        this._reset();
        this._h = initialStepSize(this._stepper, t, y, this._control.atol, this._control.rtol, this._control.stepSizeMax);
        this._t = t;
        this._history = [];
        return this;
    };
    Dopri.prototype.run = function (tEnd) {
        while (this._t < tEnd) {
            this._step();
            this._history.push(this._stepper.history.clone());
        }
        return interpolator_1.interpolator(this._history.slice(0), this._stepper, this._output);
    };
    Dopri.prototype.statistics = function () {
        return {
            lastError: this._lastError,
            nEval: this._stepper.nEval,
            nSteps: this._nSteps,
            nStepsAccepted: this._nStepsAccepted,
            nStepsRejected: this._nStepsRejected,
            stiffNNonstiff: this._stiffNNonstiff,
            stiffNStiff: this._stiffNStiff,
        };
    };
    Dopri.prototype._reset = function () {
        this._nSteps = 0;
        this._nStepsAccepted = 0;
        this._nStepsRejected = 0;
        this._stiffNStiff = 0;
        this._stiffNNonstiff = 0;
        this._lastError = 0;
    };
    Dopri.prototype._step = function () {
        var t = this._t;
        var h = this._h;
        var success = false;
        var reject = false;
        var facOld = Math.max(this._lastError, 1e-4);
        var stepControl = this._stepper.stepControl;
        var control = this._control;
        while (!success) {
            var forceThisStep = false;
            if (this._nSteps > control.maxSteps) {
                throw integrationError("too many steps", t);
            }
            if (h < control.stepSizeMin) {
                if (control.stepSizeMinAllow) {
                    h = control.stepSizeMin;
                    forceThisStep = true;
                }
                else {
                    throw integrationError("step too small", t);
                }
            }
            if (h <= Math.abs(t) * DBL_EPSILON) {
                throw integrationError("step size vanished", t);
            }
            if (t + h > control.tcrit) {
                h = control.tcrit - t;
            }
            // Carry out the step
            this._stepper.step(t, h);
            this._nSteps++;
            // Error estimation
            var err = this._stepper.error(control.atol, control.rtol);
            var fac11 = Math.pow(err, stepControl.constant);
            var facc1 = 1.0 / stepControl.factorMin;
            var facc2 = 1.0 / stepControl.factorMax;
            if (err <= 1) {
                success = true;
                this._nStepsAccepted++;
                if (this._isStiff(h)) {
                    throw integrationError("problem became stiff", t);
                }
                this._stepper.stepComplete(t, h);
                var fac = fac11 / Math.pow(facOld, stepControl.beta);
                fac = utils.constrain(fac / stepControl.factorSafe, facc2, facc1);
                var hNew = h / fac;
                this._t += h;
                if (reject) {
                    this._h = Math.min(hNew, h);
                }
                else {
                    this._h = Math.min(hNew, control.stepSizeMax);
                }
                this._lastError = err;
            }
            else {
                reject = true;
                if (this._nStepsAccepted >= 1) {
                    this._nStepsRejected++;
                }
                h /= Math.min(facc1, fac11 / stepControl.factorSafe);
            }
        }
        return this._t;
    };
    Dopri.prototype._isStiff = function (h) {
        var check = this._stiffNStiff > 0 ||
            this._nStepsAccepted % this._control.stiffCheck === 0;
        if (check) {
            if (this._stepper.isStiff(h)) {
                this._stiffNNonstiff = 0;
                if (this._stiffNStiff++ >= 15) {
                    return true;
                }
            }
            else if (this._stiffNStiff > 0) {
                if (this._stiffNNonstiff++ >= 6) {
                    this._stiffNStiff = 0;
                    this._stiffNNonstiff = 0;
                }
            }
        }
        return false;
    };
    return Dopri;
}());
exports.Dopri = Dopri;
function initialStepSize(stepper, t, y, atol, rtol, stepSizeMax) {
    // NOTE: This is destructive with respect to most of the information
    // in the object; in particular k2, k3 will be modified.
    var f0 = new Array(stepper.n);
    var f1 = new Array(stepper.n);
    var y1 = new Array(stepper.n);
    // Compute a first guess for explicit Euler as
    //   h = 0.01 * norm (y0) / norm (f0)
    // the increment for explicit euler is small compared to the solution
    stepper.rhs(t, y, f0);
    stepper.nEval++;
    var normF = 0.0;
    var normY = 0.0;
    var i = 0;
    for (i = 0; i < stepper.n; ++i) {
        var sk = atol + rtol * Math.abs(y[i]);
        normF += utils.square(f0[i] / sk);
        normY += utils.square(y[i] / sk);
    }
    var h = (normF <= 1e-10 || normY <= 1e-10) ?
        1e-6 : Math.sqrt(normY / normF) * 0.01;
    h = Math.min(h, stepSizeMax);
    // Perform an explicit Euler step
    for (i = 0; i < stepper.n; ++i) {
        y1[i] = y[i] + h * f0[i];
    }
    stepper.rhs(t + h, y1, f1);
    stepper.nEval++;
    // Estimate the second derivative of the solution:
    var der2 = 0.0;
    for (i = 0; i < stepper.n; ++i) {
        var sk = atol + rtol * Math.abs(y[i]);
        der2 += utils.square((f1[i] - f0[i]) / sk);
    }
    der2 = Math.sqrt(der2) / h;
    // Step size is computed such that
    //   h^order * max(norm(f0), norm(der2)) = 0.01
    var der12 = Math.max(Math.abs(der2), Math.sqrt(normF));
    var h1 = (der12 <= 1e-15) ?
        Math.max(1e-6, Math.abs(h) * 1e-3) :
        Math.pow(0.01 / der12, 1.0 / stepper.order);
    h = Math.min(Math.min(100 * Math.abs(h), h1), stepSizeMax);
    return h;
}

},{"./control":2,"./dopri5/stepper":6,"./interpolator":10,"./utils":12}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Dopri5StepControl = /** @class */ (function () {
    function Dopri5StepControl() {
        // For scaling during adaptive stepping
        this.factorSafe = 0.9;
        this.factorMin = 0.2; // from dopri5.f:276, retard.f:328
        this.factorMax = 10.0; // from dopri5.f:281, retard.f:333
        this.beta = 0.04;
        this.constant = 0.2 - this.beta * 0.75;
    }
    return Dopri5StepControl;
}());
exports.Dopri5StepControl = Dopri5StepControl;

},{}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var utils = require("../utils");
var control = require("./control");
var types_1 = require("../types");
// Heaps of constants!
var C2 = 0.2;
var C3 = 0.3;
var C4 = 0.8;
var C5 = 8.0 / 9.0;
var A21 = 0.2;
var A31 = 3.0 / 40.0;
var A32 = 9.0 / 40.0;
var A41 = 44.0 / 45.0;
var A42 = -56.0 / 15.0;
var A43 = 32.0 / 9.0;
var A51 = 19372.0 / 6561.0;
var A52 = -25360.0 / 2187.0;
var A53 = 64448.0 / 6561.0;
var A54 = -212.0 / 729.0;
var A61 = 9017.0 / 3168.0;
var A62 = -355.0 / 33.0;
var A63 = 46732.0 / 5247.0;
var A64 = 49.0 / 176.0;
var A65 = -5103.0 / 18656.0;
var A71 = 35.0 / 384.0;
var A73 = 500.0 / 1113.0;
var A74 = 125.0 / 192.0;
var A75 = -2187.0 / 6784.0;
var A76 = 11.0 / 84.0;
var E1 = 71.0 / 57600.0;
var E3 = -71.0 / 16695.0;
var E4 = 71.0 / 1920.0;
var E5 = -17253.0 / 339200.0;
var E6 = 22.0 / 525.0;
var E7 = -1.0 / 40.0;
// ---- DENSE OUTPUT OF SHAMPINE (1986)
var D1 = -12715105075.0 / 11282082432.0;
var D3 = 87487479700.0 / 32700410799.0;
var D4 = -10690763975.0 / 1880347072.0;
var D5 = 701980252875.0 / 199316789632.0;
var D6 = -1453857185.0 / 822651844.0;
var D7 = 69997945.0 / 29380423.0;
var Dopri5 = /** @class */ (function () {
    function Dopri5(rhs, n) {
        this.order = 5;
        this.stepControl = new control.Dopri5StepControl();
        this.nEval = 0;
        this.rhs = rhs;
        this.n = n;
        this.y = new Array(n);
        this.yNext = new Array(n);
        this.yStiff = new Array(n);
        this.k1 = new Array(n);
        this.k2 = new Array(n);
        this.k3 = new Array(n);
        this.k4 = new Array(n);
        this.k5 = new Array(n);
        this.k6 = new Array(n);
        this.history = new types_1.HistoryElement(this.order * n);
    }
    // This is the ugliest function - quite a lot goes on in here to
    // do the full step
    Dopri5.prototype.step = function (t, h) {
        var n = this.n;
        var y = this.y;
        var yNext = this.yNext;
        var k1 = this.k1;
        var k2 = this.k2;
        var k3 = this.k3;
        var k4 = this.k4;
        var k5 = this.k5;
        var k6 = this.k6;
        var hData = this.history.data;
        var i = 0;
        for (i = 0; i < n; ++i) { // 22
            yNext[i] = y[i] + h * A21 * k1[i];
        }
        this.rhs(t + C2 * h, yNext, k2);
        for (i = 0; i < n; ++i) { // 23
            yNext[i] = y[i] + h * (A31 * k1[i] + A32 * k2[i]);
        }
        this.rhs(t + C3 * h, yNext, k3);
        for (i = 0; i < n; ++i) { // 24
            yNext[i] = y[i] + h * (A41 * k1[i] + A42 * k2[i] + A43 * k3[i]);
        }
        this.rhs(t + C4 * h, yNext, k4);
        for (i = 0; i < n; ++i) { // 25
            yNext[i] = y[i] + h * (A51 * k1[i] + A52 * k2[i] + A53 * k3[i] +
                A54 * k4[i]);
        }
        this.rhs(t + C5 * h, yNext, k5);
        for (i = 0; i < n; ++i) { // 26
            this.yStiff[i] = y[i] + h * (A61 * k1[i] + A62 * k2[i] +
                A63 * k3[i] + A64 * k4[i] +
                A65 * k5[i]);
        }
        var tNext = t + h;
        this.rhs(tNext, this.yStiff, k6);
        for (i = 0; i < n; ++i) { // 27
            yNext[i] = y[i] + h * (A71 * k1[i] + A73 * k3[i] + A74 * k4[i] +
                A75 * k5[i] + A76 * k6[i]);
        }
        this.rhs(tNext, yNext, k2);
        var j = 4 * n;
        for (i = 0; i < n; ++i) {
            hData[j++] = h * (D1 * k1[i] + D3 * k3[i] + D4 * k4[i] +
                D5 * k5[i] + D6 * k6[i] + D7 * k2[i]);
        }
        for (i = 0; i < n; ++i) {
            k4[i] = h * (E1 * k1[i] + E3 * k3[i] + E4 * k4[i] +
                E5 * k5[i] + E6 * k6[i] + E7 * k2[i]);
        }
        this.nEval += 6;
    };
    Dopri5.prototype.stepComplete = function (t, h) {
        this.saveHistory(t, h);
        utils.copyArray(this.k1, this.k2); // k1 <== k2
        utils.copyArray(this.y, this.yNext); // y  <== yNext
    };
    Dopri5.prototype.error = function (atol, rtol) {
        var err = 0.0;
        var i = 0;
        for (i = 0; i < this.n; ++i) {
            var sk = atol + rtol *
                Math.max(Math.abs(this.y[i]), Math.abs(this.yNext[i]));
            err += utils.square(this.k4[i] / sk);
        }
        return Math.sqrt(err / this.n);
    };
    Dopri5.prototype.interpolate = function (t, history) {
        var hData = history.data;
        var theta = (t - history.t) / history.h;
        var theta1 = 1 - theta;
        var n = this.n;
        var ret = new Array(n);
        for (var i = 0; i < n; ++i) {
            ret[i] =
                hData[i] + theta *
                    (hData[n + i] + theta1 *
                        (hData[2 * n + i] + theta *
                            (hData[3 * n + i] + theta1 *
                                hData[4 * n + i])));
        }
        return ret;
    };
    Dopri5.prototype.isStiff = function (h) {
        var stnum = 0.0;
        var stden = 0.0;
        for (var i = 0; i < this.n; ++i) {
            stnum += utils.square(this.k2[i] - this.k6[i]);
            stden += utils.square(this.yNext[i] - this.yStiff[i]);
        }
        return stden > 0 && Math.abs(h) * Math.sqrt(stnum / stden) > 3.25;
    };
    Dopri5.prototype.reset = function (t, y) {
        for (var i = 0; i < this.n; ++i) {
            this.y[i] = y[i];
        }
        this.rhs(t, y, this.k1);
        this.nEval = 1;
    };
    Dopri5.prototype.saveHistory = function (t, h) {
        var history = this.history;
        var n = this.n;
        for (var i = 0; i < n; ++i) {
            var ydiff = this.yNext[i] - this.y[i];
            var bspl = h * this.k1[i] - ydiff;
            history.data[i] = this.y[i];
            history.data[n + i] = ydiff;
            history.data[2 * n + i] = bspl;
            history.data[3 * n + i] = -h * this.k2[i] + ydiff - bspl;
        }
        history.t = t;
        history.h = h;
    };
    return Dopri5;
}());
exports.Dopri5 = Dopri5;

},{"../types":11,"../utils":12,"./control":5}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var HistoryElement = /** @class */ (function () {
    function HistoryElement(len) {
        this.t = 0;
        this.h = 0;
        this.data = new Array(len);
    }
    HistoryElement.prototype.clone = function () {
        var h = new HistoryElement(this.data.length);
        h.t = this.t;
        h.h = this.h;
        h.data = this.data.slice(0);
        return h;
    };
    return HistoryElement;
}());
exports.HistoryElement = HistoryElement;

},{}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var dopri_1 = require("./dopri");
exports.Dopri = dopri_1.Dopri;
exports.integrate_dopri = dopri_1.integrate;
var dde_1 = require("./dde");
exports.DDE = dde_1.DDE;
exports.integrate_dde = dde_1.integrate;
var integrate_1 = require("./integrate");
exports.integrate = integrate_1.integrate;

},{"./dde":3,"./dopri":4,"./integrate":9}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var dde_1 = require("./dde");
var dopri_1 = require("./dopri");
function isRhsFn(rhs) {
    //        rhsFn: t, y, dy
    // rhsFnDelayed: t, y, dy, solution
    return rhs.length === 3;
}
function isOutputFn(output) {
    return output === null || output.length === 2;
}
function isRhsFnDelayed(rhs) {
    //        rhsFn: t, y, dy
    // rhsFnDelayed: t, y, dy, solution
    return rhs.length === 4;
}
function isOutputFnDelayed(output) {
    return output === null || output.length === 3;
}
function integrate(rhs, y, t0, t1, control, output) {
    if (control === void 0) { control = {}; }
    if (output === void 0) { output = null; }
    if (isRhsFn(rhs)) {
        if (!isOutputFn(output)) {
            throw new Error("Can't used delayed output with non-delayed rhs");
        }
        return dopri_1.integrate(rhs, y, t0, t1, control, output);
    }
    if (isRhsFnDelayed(rhs)) {
        if (!isOutputFnDelayed(output)) {
            throw new Error("Can't used non-delayed output with delayed rhs");
        }
        return dde_1.integrate(rhs, y, t0, t1, control, output);
    }
}
exports.integrate = integrate;

},{"./dde":3,"./dopri":4}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function interpolator(history, stepper, output) {
    return function (t) { return interpolate(t, history, stepper, output); };
}
exports.interpolator = interpolator;
function interpolate(t, history, stepper, output) {
    var y = [];
    // TODO: validate that 't' is increasing and fits within
    // integration time.
    var h = history;
    // TODO: need to protect us from walking off the end of the
    // array (or starting within it).
    //
    // TODO: don't allow anything to happen with a zero-length
    // history.
    var i = 0;
    for (var _i = 0, t_1 = t; _i < t_1.length; _i++) {
        var tj = t_1[_i];
        // This bit of calculation is not that nice - we're better
        // off holding both start and end times than doing this.
        while (h[i].t + h[i].h < tj) {
            i++;
        }
        var yj = stepper.interpolate(tj, h[i]);
        if (output !== null) {
            yj = yj.concat(output(tj, yj));
        }
        y.push(yj);
    }
    return y;
}

},{}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var history_1 = require("./history");
exports.HistoryElement = history_1.HistoryElement;

},{"./history":7}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var SQRT_DBL_EPSILON = Math.pow(2, -52 / 2);
function square(x) {
    return x * x;
}
exports.square = square;
// constrain x to lie in [min, max]
function constrain(x, min, max) {
    return Math.max(Math.min(x, max), min);
}
exports.constrain = constrain;
function copyArray(to, from) {
    var n = to.length;
    for (var i = 0; i < n; i++) {
        to[i] = from[i];
    }
}
exports.copyArray = copyArray;
function zeros(n) {
    var ret = Array(n);
    for (var i = 0; i < n; ++i) {
        ret[i] = 0.0;
    }
    return ret;
}
exports.zeros = zeros;
function approxEqual(x, y, tolerance) {
    if (tolerance === void 0) { tolerance = SQRT_DBL_EPSILON; }
    var xy = Math.abs(x - y);
    var xn = Math.abs(x);
    if (xn > tolerance) {
        xy /= xn;
    }
    return xy < tolerance;
}
exports.approxEqual = approxEqual;
function approxEqualArray(x, y, tolerance) {
    if (tolerance === void 0) { tolerance = SQRT_DBL_EPSILON; }
    if (y.length !== x.length) {
        throw Error("Incompatible arrays");
    }
    var scale = 0;
    var xy = 0;
    var n = 0;
    for (var i = 0; i < x.length; ++i) {
        if (x[i] !== y[i]) {
            scale += Math.abs(x[i]);
            xy += Math.abs(x[i] - y[i]);
            n++;
        }
    }
    if (n === 0) {
        return true;
    }
    scale /= n;
    xy /= n;
    if (scale > tolerance) {
        xy /= scale;
    }
    return xy < tolerance;
}
exports.approxEqualArray = approxEqualArray;
function seqLen(a, b, len) {
    var d = (a - b) / (len - 1);
    var ret = [];
    for (var i = 0; i < len; ++i) {
        var p = i / (len - 1);
        ret.push((1 - p) * a + p * b);
    }
    return ret;
}
exports.seqLen = seqLen;
// See richfitz/ring:inst/include/ring/ring.c - this is closely based
// off of this; that code was written for the same purpose.
//
// For the integration this is the problem.  We are looking for some
// history element that has a time that is at most the start time of
// our target time (i.e., element <= target).  If no element satisfies
// this, then we return -1
//
// Consider first an array of numbers.  Here we have a comparison
// function
//
//   compare = (el: number): boolean => el > target
//
// We'll have input like the following
//
//   [0, 1, 2, 3]
//
// for a target of 1.5 we're looking for '1' here as the last element
// smaller than this.
//
//   compare(0) => false
//   compare(3) => true
function search(x, compare) {
    var i0 = 0;
    var i1 = x.length - 1;
    if (x.length === 0 || compare(x[i0])) {
        return -1;
    }
    if (!compare(x[i1])) {
        // I'm not sure here if this should be x.length or i1
        return i1;
    }
    // from this point, we will always have:
    //
    //   compare(x[i0]) => false
    //   compare(x[i1]) => true
    while (i1 - i0 > 1) {
        var i2 = Math.floor((i0 + i1) / 2);
        if (compare(x[i2])) {
            i1 = i2;
        }
        else {
            i0 = i2;
        }
    }
    return i0;
}
exports.search = search;

},{}]},{},[1]);
