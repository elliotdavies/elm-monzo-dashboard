require('./main.css');

var Elm = require('./Main.elm');
var root = document.getElementById('root');


// This should come from a separate file
var env = {
  clientId: "oauthclient_00009Cq3YjQYO4voDQt7Ev",
  clientSecret: "/BwwMQXNYLcFgbfitp+9cDYs3vS/ohHlf/1L7Ak3jT2C2TOR5ThGujaVN0WwfAViJztytE7LnaR7lll0mRJR"
}


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
