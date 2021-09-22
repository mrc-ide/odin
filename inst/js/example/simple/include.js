function odinInit() {
    var model = Object.values(odin)[0];
    var pars = model.prototype.coef;
    var container = document.getElementById("odin_parameters");
    Object.keys(pars).forEach(function(name) {
        odinControlAdd(name, pars, container)
    });
    odinPlot();
}

function odinPlot() {
    var graph = document.getElementById("odin_graph");
    var t0 = parseFloat(document.getElementById("t0").value);
    var t1 = parseFloat(document.getElementById("t1").value);
    var width = parseInt(graph.style.width, 10);
    var dt = 1.0 * (t1 - t0) / width;
    var t = [];
    for (var i = 0; i < width; ++i) {
        t.push(t0 + i * dt);
    }

    var model = Object.values(odin)[0];
    var user = odinParameters(model);
    var mod = new model(user);
    var res = mod.run(t);
    new Dygraph(graph, res.y, {"labels": res.names});
}

function odinParameters(model) {
    var user = {};
    Object.keys(model.prototype.coef).forEach(function(name) {
        var el = document.getElementById(odinParameterId(name));
        user[name] = parseFloat(el.value);
    });
    return user;
}


function odinControlAdd(name, data, container) {
    var label = document.createElement("code");
    label.textContent = name;

    var input = document.createElement("input");
    input.type = "text";
    input.id = odinParameterId(name);
    if (data[name]["default"] !== null) {
        input.value = data[name]["default"];
    }

    container.appendChild(label);
    container.appendChild(input);
    container.appendChild(document.createElement("br"));
}

function odinParameterId(name) {
    return "odin_parameter_" + name;
}
