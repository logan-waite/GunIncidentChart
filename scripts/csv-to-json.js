import fs from 'fs';
import _ from 'lodash/fp.js';

const [, , csvPath, targetPath] = process.argv;

main(csvPath, targetPath);

function toInt(value) {
    return /^[-+]?(\d+|Infinity)$/.test(value) ? Number(value) : NaN;
}

function ifElse(predicate, ifPath, elsePath) {
    return function doIfElse(val) {
        return predicate(val) ? ifPath(val) : elsePath(val)
    }
}

function prepData(string) {
    return _.pipe(
        _.replace(/\r\n/g, '\n'),
        _.split('\n'),
        _.map(
            _.pipe(
                _.split(/(?!,\s),/),
                _.map(
                    _.replace(/"/g, '')
                )
            )
        )
    )(string);
}

function getHeaders(data) {
    return _.pipe(_.head, _.map(_.camelCase))(data);
}

function propify(prop, val) {
    const intVal = toInt(val)
    console.log({ prop, val, intVal });
    return `"${prop}":${_.isInteger(intVal) ? intVal : `"${val}"`}`;
}

function createJsonObject(headers) {
    return function _createJsonObject(list) {
        return _.pipe(
            _.zipWith(propify, headers),
            _.join(','),
            (string) => `{${string}}`
        )(list);
    }
}

function convertToJson(headers, data) {
    return _.pipe(
        _.tail,
        _.map(createJsonObject(headers)),
        _.join(','),
        (string) => `[${string}]`
    )(data);
}

function main(csvPath, targetPath) {
    try {
        const fileString = fs.readFileSync(csvPath, { encoding: 'utf-8' });

        const data = prepData(fileString);
        const headers = getHeaders(data);
        const json = convertToJson(headers, data);

        fs.writeFileSync(targetPath, json, 'utf8');
    } catch (err) {
        throw new Error(err)
    }
}

const reportPath = './incidents.csv';



