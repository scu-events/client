import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

let app = Main.init({
  node: document.getElementById("root"),
  flags: "http://localhost:4000" // process.env.BACKEND_URL
});

registerServiceWorker();
