const path = require('path');
const fs = require('fs');

const directoryPath = path.join(__dirname, 'icons');

fs.readdir(directoryPath, function (err, files) {
  if (err) {
    return console.error('Unable to scan directory: ' + err);
  }
  const moduleName = "Icons"

  logDim("Parsing icon files...")
  const svgData = map(parse, sort(files))

  logDim("Generating elm file...")
  const icons = map(prop('name'), svgData)
  const iconString = join(', ')(icons)

  const attributes = map(prop('attribute'), svgData)
  const attributeString = compose(join(', '), sort)([...new Set(attributes)])

  const codeBlocks = map(createCodeBlock, svgData)
  const fileBody = reduce(appendBlockToFile, fileHeader(moduleName, iconString, attributeString), codeBlocks)

  const targetDir = path.join(__dirname, 'src', 'UI', `${moduleName}.elm`)
  fs.writeFile(targetDir, fileBody, logError)

  logGreen(`Successfully generated ${files.length} icons into ${targetDir}`)

});

function appendBlockToFile (block, file) {
  return file + "\n\n" + block
}

function createCodeBlock (svg) {
  const typeAnnotation = `${svg.name} : Int -> Color -> Element msg`;
  const declaration = `${svg.name} =`
  const body = `    icon "${svg.viewbox}" [ Svg.path [ ${svg.attribute} "${svg.data}" ] [] ]`

  return `${typeAnnotation}\n${declaration}\n${body}\n`
}

function parse (file) {
  const fileContents = fs.readFileSync(`${directoryPath}/${file}`, { encoding: 'utf-8' })
  const viewbox = fileContents.match(/viewbox="(\d+ \d+ \d+ \d+)"/i)[1];
  const [_, attribute, data] = fileContents.match(/path\s([a-zA-Z]+)="(.+)"/i)
  // TODO: perhaps convert dashed names to camel case
  const name = file.slice(0, -4)
  return { viewbox, attribute, data, name }
}

function fileHeader (moduleName, icons, attributes) {
  return `{-
    This file is a generated file. 
    Any changes made will be lost 
    on regeneration
-}

module UI.${moduleName} exposing (${icons})


import Element exposing (Color, Element)
import Libraries.Icons exposing (icon)
import Svg
import Svg.Attributes exposing (${attributes})

`
}

function logError (err) {
  if (err) {
    console.error(err)
    return
  }
}

/**********************/
/** Helper Functions **/
/**********************/

function prop (idx) {
  return function (obj) {
    return obj[idx]
  }
}

// (a -> b) -> (b -> c) -> a -> c
function compose (f, g) {
  return function composed (x) {
    return f(g(x))
  }
}

// String -> List String -> String
function join (connector) {
  return function (list) {
    return list.join(connector)
  }
}

// List a -> List a
const sort = sortWith((a, b) => {
  if (a > b) return 1
  else if (a < b) return -1
  else return 0
})

// (a -> a -> Int) -> List a -> List a
function sortWith (comparator) {
  return function (list) {
    return Array.prototype.slice.call(list, 0).sort(comparator)
  }
}

// (a -> b -> b) -> b -> List a -> b
function reduce (fn, result, list) {
  var idx = 0
  var len = list.length
  while (idx < len) {
    result = fn(list[idx], result)
    idx += 1
  }
  return result
}

// (a -> b) -> List a -> List b
function map (fn, functor) {
  var idx = 0
  var len = functor.length
  var result = Array(len)
  while (idx < len) {
    result[idx] = fn(functor[idx])
    idx += 1
  }
  return result
}

function logger (style) {
  return function styledLogger (string) {
    console.log(`${style}%s\x1b[0m`, string)
  }
}

const logGreen = logger("\x1b[32m")
const logDim = logger("\x1b[2m");