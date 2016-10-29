require('./main.css');

var Elm = require('./Main.elm');
var root = document.getElementById('root');

var env = require('json!../env.json');

function getItem(key) {
  return localStorage ? localStorage.getItem(key) : null;
}


function setItem(key, value) {
  if (localStorage) localStorage.setItem(key, value);
}


function deleteItem(key) {
  if (localStorage) localStorage.removeItem(key);
}


var app = Elm.Main.embed(root, {
  clientId: env.clientId,
  clientSecret: env.clientSecret,
  url: window.location.href,
  stateToken: getItem('stateToken'),
  accessToken: getItem('accessToken')
});


app.ports.storeStateToken.subscribe(function(token){
  setItem('stateToken', token);
});


app.ports.deleteStateToken.subscribe(function(){
  deleteItem('stateToken');
});


app.ports.storeAccessToken.subscribe(function(token){
  setItem('accessToken', token);
});


app.ports.deleteAccessToken.subscribe(function(){
  deleteItem('accessToken');
});
