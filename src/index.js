import { Elm } from "./Main.elm"

var storedModel = {};

try {
    storedModel = JSON.parse(localStorage.getItem("model"));
} catch (e) {
    // Either the parse failed, or localStorage is inaccessible,
    // or we have no model. It's fine. Whatever.
}

var app = Elm.Main.init({
    node: document.getElementById("main"),
    flags: storedModel
});

app.ports.save.subscribe((model) =>
    localStorage.setItem("model", JSON.stringify(model))
);
