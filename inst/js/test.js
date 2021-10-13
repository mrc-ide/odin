// code for use with a test model:
function run(name, user, t, y, control) {
    var mod = new odin[name](user);
    return mod.run(t, y, control);
}
