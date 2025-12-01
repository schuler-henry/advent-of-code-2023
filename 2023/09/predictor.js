const fs = require("fs");

function getDifferences(list) {
  const differences = [];

  for (let i = 1; i < list.length; i++) {
    differences.push(list[i] - list[i - 1]);
  }
  return differences;
}

function isListZero(list) {
  return list.every((elem) => elem === 0);
}

function extrapolateEnd(lowerList, upperList) {
  upperList.push(
    upperList[upperList.length - 1] + lowerList[lowerList.length - 1]
  );
}

function extrapolateStart(lowerList, upperList) {
  upperList.unshift(upperList[0] - lowerList[0]);
}

function processInput(input) {
  const calculatedSequences = [];
  calculatedSequences.push(input);

  while (!isListZero(calculatedSequences[calculatedSequences.length - 1])) {
    const nextSequence = getDifferences(
      calculatedSequences[calculatedSequences.length - 1]
    );
    calculatedSequences.push(nextSequence);
  }

  // Extrapolate
  for (let i = calculatedSequences.length - 2; i >= 0; i--) {
    extrapolateStart(calculatedSequences[i + 1], calculatedSequences[i]);
    extrapolateEnd(calculatedSequences[i + 1], calculatedSequences[i]);
  }

  return [
    calculatedSequences[0][0],
    calculatedSequences[0][calculatedSequences[0].length - 1],
  ];
}

const fileName = process.argv[2] || "input.txt";
const file = fs.readFileSync(fileName, "utf-8");

const lines = file.split("\n").map((line) => line.trim());

let resultStart = 0;
let resultEnd = 0;

for (const line of lines) {
  const numbers = line.split(" ").map((num) => parseInt(num, 10));

  [extrapolatedStart, extrapolatedEnd] = processInput(numbers);
  resultStart += extrapolatedStart;
  resultEnd += extrapolatedEnd;
}

console.log("Final Result: ", resultStart, resultEnd);
