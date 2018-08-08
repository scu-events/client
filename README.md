### `elm-app build`
Builds the app for production to the `build` folder.

The build is minified, and the filenames include the hashes.
Your app is ready to be deployed!

### `elm-app start`
Runs the app in the development mode.
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.
You will also see any lint errors in the console.

### `elm-app install`

An alias for [`elm-app package install`](#package)

### `elm-app test`
Run tests with [node-test-runner](https://github.com/rtfeldman/node-test-runner/tree/master)

You can make test runner watch project files by running:
```sh
elm-app test --watch
```

## Using custom environment variables

In your JavaScript code you have access to variables declared in your
environment, like an API key set in an `.env`-file or via your shell. They are
available on the `process.env`-object and will be injected during build time.

Besides the `NODE_ENV` variable you can access all variables prefixed with
`ELM_APP_`:

```bash
# .env
ELM_APP_API_KEY="secret-key"
```

Alternatively, you can set them on your shell before calling the start- or
build-script, e.g.:

```bash
ELM_APP_API_KEY="secret-key" elm-app start
```

Both ways can be mixed, but variables set on your shell prior to calling one of
the scripts will take precedence over those declared in an `.env`-file.

Passing the variables to your Elm-code can be done via `flags`:

```javascript
// index.js
import { Main } from './Main.elm';

Main.fullscreen({
  environment: process.env.NODE_ENV,
  apiKey: process.env.ELM_APP_API_KEY,
});
```

```elm
-- Main.elm
type alias Flags = { apiKey : String, environment : String }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ...

main =
  programWithFlags { init = init, ... }
```

Be aware that you cannot override `NODE_ENV` manually. See
[this list from the `dotenv`-library](https://github.com/bkeepers/dotenv#what-other-env-files-can-i-use)
for a list of files you can use to declare environment variables.

### GitHub Pages

#### Step 1: Add `homepage` to `elm-package.json`

**The step below is important!**<br>
**If you skip it, your app will not deploy correctly.**

Open your `elm-package.json` and add a `homepage` field:

```js
  "homepage": "https://myusername.github.io/my-app",
```

Create Elm App uses the `homepage` field to determine the root URL in the built HTML file.

#### Step 2: Build the static site

```sh
elm-app build
```

#### Step 3: Deploy the site by running `gh-pages -d build`

We will use [gh-pages](https://www.npmjs.com/package/gh-pages) to upload the files from the `build` directory to GitHub. If you haven't already installed it, do so now (`npm install -g gh-pages`)

Now run:

```sh
gh-pages -d build
```

#### Step 4: Ensure your project’s settings use `gh-pages`

Finally, make sure **GitHub Pages** option in your GitHub project settings is set to use the `gh-pages` branch:

<img src="http://i.imgur.com/HUjEr9l.png" width="500" alt="gh-pages branch setting">

#### Step 5: Optionally, configure the domain

You can configure a custom domain with GitHub Pages by adding a `CNAME` file to the `public/` folder.

#### Notes on client-side routing

GitHub Pages doesn’t support routers that use the HTML5 `pushState` history API under the hood (for example, React Router using `browserHistory`). This is because when there is a fresh page load for a url like `http://user.github.io/todomvc/todos/42`, where `/todos/42` is a frontend route, the GitHub Pages server returns 404 because it knows nothing of `/todos/42`. If you want to add a router to a project hosted on GitHub Pages, here are a couple of solutions:

* You could switch from using HTML5 history API to routing with hashes.
* Alternatively, you can use a trick to teach GitHub Pages to handle 404 by redirecting to your `index.html` page with a special redirect parameter. You would need to add a `404.html` file with the redirection code to the `build` folder before deploying your project, and you’ll need to add code handling the redirect parameter to `index.html`. You can find a detailed explanation of this technique [in this guide](https://github.com/rafrex/spa-github-pages).
