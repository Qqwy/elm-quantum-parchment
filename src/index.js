// Styles
require('./assets/styles/main.scss');

// Vendor JS is imported as an entry in webpack.config.js

// Elm
var Elm = require('./elm/Main.elm').Elm;
console.log(require('./elm/Main.elm'));

const storageKey = "QuantumParchment";

var app = Elm.Main.init({node: "app-container", flags: {}});
console.log(app, loadStorage());

app.ports.persistToStorage.subscribe(
    function(data){
        window.localStorage.setItem(storageKey, data);
    });

function loadStorage(){
    return window.localStorage.getItem(storageKey) || "";
}

function updateStorage(){
    app.ports.storageUpdate.send(loadStorage());
}

addEventListener('storage', updateStorage, false);
