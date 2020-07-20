import './main.css';
import { Elm } from './Main.elm';


const urlParams = new URLSearchParams(window.location.search);

console.log( urlParams.get('country') || "SE")

Elm.Main.init({
  flags: {country: urlParams.get('country') || "SE", showDash: urlParams.get('country') === null},
  node: document.getElementById('root')
});
