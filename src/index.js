require('./main.scss');

const { Elm } = require('./Main.elm');
const mountNode = document.getElementById('main');


const storedState = localStorage.getItem('shuffle-save-v1');
const startingState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({
  node: mountNode,
  flags: startingState
});

app.ports.setStorage.subscribe((state) => {
  localStorage.setItem('shuffle-save-v1', JSON.stringify(state));
})
