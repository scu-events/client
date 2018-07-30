import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import moment from 'moment';

console.log(process.env)
let app = Main.embed(document.getElementById('root'), { backendURL: process.env.ELM_APP_BACKEND_URL });

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

document.addEventListener('DOMContentLoaded', () => {
  const navbarBurger = document.getElementById("navbar-burger");
  navbarBurger.addEventListener('click', () => {

    // Get the target from the "data-target" attribute
    const target = document.getElementById(navbarBurger.dataset.target);

    // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
    navbarBurger.classList.toggle('is-active');
    target.classList.toggle('is-active');

  });
});

registerServiceWorker();
