# proact-starter
Proact starter application with hot-reloading, bundling, routing and enzyme testing.

This starter app is a skeleton project using Purescript's [Proact](https://github.com/alvart/proact). It's configured with webpack and supports hot-reloading, enzyme testing and page routing.

## Installation

Clone the repository and run `npm install` to get started:

```sh
git clone git://github.com/alvart/proact-starter.git proact-starter
cd proact-starter
npm install
npm start
```

After compiling, the app should be available at `http://localhost:3000`.

### Directory structure

- `src`: Application source code.
  - `src/app/Document.js` & `src/app/Document.purs`: I was not able to find a core API that changes the document's title (how is that possible?!) so I had to write my own simple FFI function that does it.
  - `src/app/FilterMenu.purs`: The component for the filter menu.
  - `src/app/Main.purs`: Runs the main component (todo).
  - `src/app/ProactPlus.purs`: Utility functions that this particular app (but maybe not others!!) wishes Proact would have provided out of the box.
  - `src/app/Router.purs`: Parses the URL and maintains the current path of the application.
  - `src/app/Task.purs`: The component for the to-do task. It represents a row inside the to-do table.
  - `src/app/Todo.purs`: The component for the to-do task list. It is the main and only component of the page.
  - `src/index.html`: The web page hosting the application.
  - `src/index.js`: The entry point to the To-Do application.
- `test`: Application test code.
  - `test/app/Main.purs`: Defines the test suites for all the application components.
  - `test/app/FilterMenu.purs`: The enzyme tests for the filter menu component.
  - `test/app/Task.purs`: The enzyme tests for the task component.
  - `test/app/Todo.purs`: The enzyme tests for the todo component.
  - `test/index.js`: The entry point for the test runner.

### NPM scripts

#### start

`npm start` will start a development server, which hot-reloads the application when source code files change.

#### test

`npm run test` will start [Karma](https://github.com/karma-runner/karma) and launch Chrome to run the three tests included in this project.

The tests make use of the [Enzyme](https://github.com/airbnb/enzyme) library which technically is not a unit test framework (side-effects might be limited yet they still exist) but it's very convenient for testing React components such as those defined using Proact. Plus, integration and system testing is straightforward!

Once the browser is launched, Karma keeps watching the test and source code files for any changes and will re-run the tests automatically. Plus, when a test case fails, the error trace will point to the exact location in the purescript code where the exception was thrown.

As a side note, **do NOT run `test`, `start` and/or `build` while any of these are already running.**

#### build

`npm run build` builds the application bundle. Source-maps appear to be broken when using UglifyJS and Purescript so they're disabled for this mode.
