import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import moment from 'moment';

let app = Main.init({
  node: document.getElementById("root"),
  flags: "http://localhost:4000" // process.env.BACKEND_URL
});

// TODO refactor
app.ports.populateCalendar.subscribe((day) => {
  let date = moment(day, "YYYY MMM DD")

  let lastMonthDays = date.startOf('month').day() % 7
  let calendar = Array.from(Array(lastMonthDays)).map((x, i) => (
      date.clone().subtract(i + 1, 'day')
    )).reverse().concat(Array.from(Array(35 - lastMonthDays)).map((x, i) => (
         date.clone().add(i, 'day')
      ))).map((d) => d.format())

  app.ports.onPopulateCalendar.send(calendar);
});

// TODO refactor
app.ports.repopulateCalendar.subscribe(([day, n]) => {
  let date = moment(day, "YYYY MMM DD").add(n, 'month')

  let lastMonthDays = date.startOf('month').day() % 7
  let calendar = Array.from(Array(lastMonthDays)).map((x, i) => (
      date.clone().subtract(i + 1, 'day')
    )).reverse().concat(Array.from(Array(35 - lastMonthDays)).map((x, i) => (
         date.clone().add(i, 'day')
      ))).map((d) => d.format())

  app.ports.onPopulateCalendar.send(calendar);
});

registerServiceWorker();
