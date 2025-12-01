const DEBUG = false;

function unfoldSpringOder(springOrder) {
  let unfolded = springOrder;
  // four times repeat
  for (let i = 0; i < 1; i++) {
    unfolded += "?" + springOrder;
  }

  return unfolded;
  // let unfold = springOrder;
  // if (!springOrder.endsWith("#")) {
  //   unfold = "?" + springOrder + "?";
  // } else {
  //   unfold = "" + springOrder + "?";
  // }
  // return unfold;
}

function unfoldConfigurationSize(configurationSize) {
  let unfolded = configurationSize;
  // four times repeat
  for (let i = 0; i < 1; i++) {
    unfolded += "," + configurationSize;
  }

  return unfolded;
  // return configurationSize;
}

function readInput(fileName, unfold = false) {
  const fs = require("fs");
  const file = fs.readFileSync(fileName, "utf8");
  const lines = file.trim().split("\n");

  const springRows = [];
  for (const line of lines) {
    let [springOrder, configurationSizes] = line.split(" ");

    if (unfold) {
      springOrder = unfoldSpringOder(springOrder);
      configurationSizes = unfoldConfigurationSize(configurationSizes);
    }

    const springRow = {
      springOrder: springOrder,
      configurationSizes: configurationSizes.split(",").map(Number),
      numOfUnknownFields: (springOrder.match(/\?/g) || []).length,
      numOfKnownDefectPlaces: (springOrder.match(/\#/g) || []).length,
      totalNumOfDefects: configurationSizes
        .split(",")
        .map(Number)
        .reduce((a, b) => a + b, 0),
      minNumOfRequiredFields:
        configurationSizes
          .split(",")
          .map(Number)
          .reduce((a, b) => a + b, 0) +
        configurationSizes.split(",").length -
        1,
      minGroupSizes: springOrder
        .split(/[\.\?]/)
        .map((group) => group.length)
        .filter((size) => size > 0),
      maxGroupSizes: springOrder
        .split(".")
        .map((group) => group.length)
        .filter((size) => size > 0),
      groupIntervals: springOrder
        .split(".")
        .map((group) => {
          return {
            low: Math.max(
              ...group.split("?").map((subgroup) => subgroup.length)
            ),
            high: group.length,
          };
        })
        .filter((interval) => interval.high > 0),
      regex: configurationSizes
        .split(",")
        .map(Number)
        .reduce((acc, size) => acc + "#".repeat(size) + "[\\.]+", "[\\.]*")
        .replace(/\+$/, "*"),
      optionalIndices: getIndices(springOrder, "?"),
    };

    springRows.push(springRow);
  }

  return springRows;
}

function getIndices(springOrder, char) {
  const optionalIndices = [];
  for (let i = 0; i < springOrder.length; i++) {
    if (springOrder[i] === char) {
      optionalIndices.push(i);
    }
  }
  return optionalIndices;
}

function bruteForceSolver(springRow) {
  const numOfMissingDefects =
    springRow.totalNumOfDefects - springRow.numOfKnownDefectPlaces;

  if (numOfMissingDefects === 0) {
    const testRow = springRow.springOrder.replaceAll("?", ".");
    if (testRow.match(new RegExp(springRow.regex))) {
      return 1;
    } else {
      return 0;
    }
  }

  return recursion(
    springRow.springOrder,
    springRow.optionalIndices,
    numOfMissingDefects - 1,
    springRow.regex
  );

  // deepclone string
}

function recursion(currentRow, optionalIndices, nesting, regex) {
  let hits = 0;
  if (DEBUG) {
    console.log(
      "Number of missing defects to place:",
      nesting,
      optionalIndices
    );
  }
  for (let index = 0; index < optionalIndices.length - nesting; index++) {
    if (DEBUG) {
      console.log("Nesting: ", nesting, "Index:", index);
    }
    let clonedRow =
      currentRow.substring(0, optionalIndices[index]) +
      "#" +
      currentRow.substring(optionalIndices[index] + 1);

    if (nesting !== 0) {
      hits += recursion(
        clonedRow,
        optionalIndices.slice(index + 1),
        nesting - 1,
        regex
      );
    } else {
      const testRow = clonedRow.replaceAll("?", ".");
      if (DEBUG) {
        console.log("Checking row:", testRow, "against regex:", regex);
      }
      if (testRow.match(new RegExp(regex))) {
        hits += 1;
      }
    }
  }

  return hits;
}

// Main execution

const fileName = process.argv[2] || "input.txt";

function main(unfold = false) {
  const springRows = readInput(fileName, unfold);

  if (DEBUG) {
    for (const row of springRows) {
      console.log("Spring row analysis:", row);
    }
  }

  const results = [];

  for (const springRow of springRows) {
    // console.log(springRow);
    const numOfSolutions = bruteForceSolver(springRow);
    console.log("Number of valid configurations:", numOfSolutions);
    results.push(numOfSolutions);
  }

  const accumulatedResult = results.reduce((a, b) => a + b, 0);
  console.log("Accumulated result:", accumulatedResult);

  return results;
}

const res1 = main();
const res2 = main(true);

console.log(res1, res2);
const resFinal = res1.map((val, idx) => {
  // return val * Math.pow(res2[idx], 4);
  return val * Math.pow(res2[idx] / val, 4);
});

console.log("Final result:", resFinal);
const finalAccumulated = resFinal.reduce((a, b) => a + b, 0);
console.log("Final accumulated result:", finalAccumulated);
