#!/usr/bin/env node

const express = require('express');

const app = express();
const fs = require('fs');
const path = require('path');
const cors = require('cors');
const url = require('url');

const readFile = filePath => fs.readFileSync(filePath, 'utf-8');

const cwd = process.cwd();

// console.log(elmPackageContents);

const elmNurseryRoot = path.resolve(__dirname);
console.log(elmNurseryRoot);
const staticPath = path.resolve(path.join(elmNurseryRoot, '../client/build'));
console.log(staticPath);

app.use(cors());

app.get('/api/elm-source-directories/', (request, response) => {
  const elmPackageJsonPath = path.join(cwd, 'elm-package.json');
  const elmPackageContents = readFile(elmPackageJsonPath);
  const elmPackageData = JSON.parse(elmPackageContents);
  const sourceDirectories = elmPackageData['source-directories'];
  response.json(sourceDirectories);
});

app.get('/api/directory-contents/', (request, response) => {
  const urlParts = url.parse(request.url, true);
  const { query } = urlParts;
  const { directory } = query;

  const fullDirectoryPath = path.join(cwd, directory);
  console.log(fullDirectoryPath);

  const files = fs.readdirSync(fullDirectoryPath, 'utf-8').reduce((acc, fileName) => {
    const fullFilePath = path.join(fullDirectoryPath, fileName);
    if (fs.lstatSync(fullFilePath).isDirectory()) {
      return acc.concat([{ fileName, isDirectory: true }]);
    } else if (path.extname(fileName) === '.elm') {
      return acc.concat([{ fileName, isDirectory: false }]);
    }
    return acc;
  }, []);
  response.json({ files, directory });
});

app.use(express.static(staticPath));

app.listen(3000, () => {
  console.log('Example app listening on port 3000!');
});
