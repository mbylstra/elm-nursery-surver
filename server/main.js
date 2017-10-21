#!/usr/bin/env node

const express = require("express");
// const jsonErrorHandler = require("express-json-error-handler");

const app = express();
const fs = require("fs");
const path = require("path");
const cors = require("cors");
const url = require("url");
const EventEmitter = require("events");
const { execFileSync } = require("child_process");
const stream = require("stream");

const readFile = filePath => fs.readFileSync(filePath, "utf-8");

const cwd = process.cwd();

// console.log(elmPackageContents);

const elmNurseryRoot = path.resolve(__dirname);
console.log(elmNurseryRoot);
const staticPath = path.resolve(path.join(elmNurseryRoot, "../client/build"));
console.log(staticPath);

// Crazy error handling stuff
class ExceptionEmitter extends EventEmitter {}
const exceptionEmitter = new ExceptionEmitter();
process.on("uncaughtException", err => {
  exceptionEmitter.emit("err", err);
});

app.use(cors());
// app.use(jsonErrorHandler());

app.get("/api/elm-source-directories/", (request, response) => {
  const elmPackageJsonPath = path.join(cwd, "elm-package.json");
  const elmPackageContents = readFile(elmPackageJsonPath);
  const elmPackageData = JSON.parse(elmPackageContents);
  const sourceDirectories = elmPackageData["source-directories"];
  response.json(sourceDirectories);
});

app.get("/api/directory-contents/", (request, response) => {
  const urlParts = url.parse(request.url, true);
  const { query } = urlParts;
  const { directory } = query;

  const fullDirectoryPath = path.join(cwd, directory);
  console.log(fullDirectoryPath);

  const files = fs
    .readdirSync(fullDirectoryPath, "utf-8")
    .reduce((acc, fileName) => {
      const fullFilePath = path.join(fullDirectoryPath, fileName);
      if (fs.lstatSync(fullFilePath).isDirectory()) {
        return acc.concat([{ fileName, isDirectory: true }]);
      } else if (path.extname(fileName) === ".elm") {
        return acc.concat([{ fileName, isDirectory: false }]);
      }
      return acc;
    }, []);
  response.json({ files, directory });
});

app.get("/api/elm-module-view-functions/", (request, response) => {
  // Current it's in do it once mode!

  const urlParts = url.parse(request.url, true);
  const { query } = urlParts;
  const { filePath } = query;
  const fullSourceFilePath = path.join(cwd, filePath);

  // load Elm module
  const elm = requireUncached("./find-view-functions-elm-app.js");

  const elmFlags = { sourceCode: readFile(fullSourceFilePath) };

  // get Elm ports
  const { ports } = elm.FindViewFunctionsApp.worker(elmFlags);

  ports.return.subscribe(viewFunctions => {
    response.json(viewFunctions);
  });
});

app.get("/api/view-function/", (request, response) => {
  // Current it's in do it once mode!

  const urlParts = url.parse(request.url, true);
  const { query } = urlParts;
  const { elmModulePath, viewFunction } = query;
  const fullSourceFilePath = path.join(cwd, elmModulePath);

  // load Elm module
  const elm = requireUncached("./elm.js");

  const elmFlags = {
    subjectSourceCode: readFile(fullSourceFilePath),
    functionName: viewFunction,
    elmPackageContents: readFile("elm-package.json"),
    exactDependenciesContents: readFile("elm-stuff/exact-dependencies.json")
  };

  // get Elm ports
  console.log("firing up elm worker");
  const { ports } = elm.Main.worker(elmFlags);

  ports.readElmPackageInfoContents.subscribe(elmPackageJsonPaths => {
    ports.readElmPackageInfoContentsResult.send(
      elmPackageJsonPaths.map(path => [path, fs.readFileSync(path, "utf8")])
    );
  });

  ports.readElmModule.subscribe(data => {
    const moduleAttempts = new Map();
    let returnedData = null;

    const moduleName = data.portScope.moduleName;
    if (moduleAttempts.has(moduleName)) {
      moduleAttempts.set(moduleName, moduleAttempts.get(moduleName) + 1);
    } else {
      moduleAttempts.set(moduleName, 1);
    }

    if (fs.existsSync(data.path)) {
      const contents = fs.readFileSync(data.path, "utf8");
      returnedData = {
        portScope: data.portScope,
        maybeContents: contents
      };
    } else {
      returnedData = {
        portScope: data.portScope,
        maybeContents: null
      };
    }
    printReturnedDataInfo(returnedData);
    console.log("modules attempted", moduleAttempts);
    ports.readElmModuleResult.send(returnedData);
  });

  function printReturnedDataInfo(returnedData) {
    console.log("JS portScope", returnedData.portScope);
    console.log("JS foundModule/hasContents", returnedData.contents != null);
  }

  exceptionEmitter.on("err", err => {
    errString = err.toString();
    delete elm;
    delete ports;
    // console.log(err.toString());
    console.log("caught an error inside elm stuff!;");
    // response.json();
    exceptionEmitter.removeAllListeners();
    response.status(500).send(errString);
  });

  ports.writeOutput.subscribe(viewFunctions => {
    viewFunctionCode = viewFunctions[0][1];
    // formattedViewFunctions = viewFunctions.map(([name, code]) => [
    //   name,
    //   formatCode(code)
    // ]);
    formattedViewFunction = formatCode(viewFunctionCode);
    response.json(formattedViewFunction);
  });
});

app.use(express.static(staticPath));

app.listen(3000, () => {
  console.log("Example app listening on port 3000!");
});

function requireUncached(module) {
  delete require.cache[require.resolve(module)];
  return require(module);
}

function formatCode(code) {
  return execFileSync("elm-format", ["--stdin"], {
    input: code,
    encoding: "utf-8"
  });
}
