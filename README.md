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
  - `src/app/*`: A demo application (to-do list) included with this package.
  - `src/index.html`: The web page hosting the application.
  - `src/index.js`: The entry point to the demo application.
- `test`: Application test code.
  - `test/app/*`: The enzyme tests for the demo application.
  - `test/index.js`: The entry point for the test runner.

### NPM scripts

#### start

`npm start` will start a development server, which hot-reloads the application when code files change.

#### test

`npm run test` will start [Karma](https://github.com/karma-runner/karma) and launch Chrome to run the three tests included in this project.

The tests make use of the [Enzyme](https://github.com/airbnb/enzyme) library which technically is not a unit test framework (side-effects might be limited yet they still exist) but it's very convenient for testing React components such as those defined using Proact. Plus, integration and system testing is straightforward!

Once the browser is launched, Karma keeps watching the test and source code files for any changes and will re-run the tests automatically. Plus, when a test case fails, the error trace will point to the exact location in the purescript code where the exception was thrown.

**Do NOT run `test`, `start` and/or `build` if any of these are already running.**

#### build

`npm run build` builds the application bundle. Source-maps appear to be broken when using UglifyJS and Purescript so they're disabled for this mode.
