function readFileToMap(filePath) {
  const fs = require("fs");
  const content = fs.readFileSync(filePath, "utf-8");
  const lines = content.split("\n").map((line) => line.trim());

  const map = [];

  for (const line of lines) {
    map.push(line.split(""));
  }
  return map;
}

function getEmptyRowIndices(map) {
  const emptyRowIndices = [];

  for (let y = 0; y < map.length; y++) {
    if (map[y].every((cell) => cell === ".")) {
      emptyRowIndices.push(y);
    }
  }

  return emptyRowIndices;
}

function getEmptyColumnIndices(map) {
  return getEmptyRowIndices(transposeMap(map));
}

function transposeMap(map) {
  const transposedMap = [];

  for (let x = 0; x < map[0].length; x++) {
    const newRow = [];
    for (let y = 0; y < map.length; y++) {
      newRow.push(map[y][x]);
    }
    transposedMap.push(newRow);
  }
  return transposedMap;
}

function getGalaxyPositions(map) {
  const positions = [];

  for (let y = 0; y < map.length; y++) {
    for (let x = 0; x < map[0].length; x++) {
      if (map[y][x] === "#") {
        positions.push({ id: positions.length + 1, x: x, y: y });
      }
    }
  }

  return positions;
}

function calculateShortestPath(
  posA,
  posB,
  emptyRowIndices,
  emptyColumnIndices,
  replaceFactor
) {
  const xPositions = [posA.x, posB.x].sort((a, b) => a - b);
  if (DEBUG) {
    console.log("X Positions:", xPositions);
  }
  const numOfEmptyColumnsBetweenXCoords = emptyColumnIndices.filter(
    (colIndex) => colIndex > xPositions[0] && colIndex < xPositions[1]
  ).length;

  const yPositions = [posA.y, posB.y].sort((a, b) => a - b);
  if (DEBUG) {
    console.log("Y Positions:", yPositions);
  }
  const numOfEmptyRowsBetweenYCoords = emptyRowIndices.filter(
    (rowIndex) => rowIndex > yPositions[0] && rowIndex < yPositions[1]
  ).length;

  return (
    Math.abs(posA.x - posB.x) +
    numOfEmptyColumnsBetweenXCoords * (replaceFactor - 1) +
    Math.abs(posA.y - posB.y) +
    numOfEmptyRowsBetweenYCoords * (replaceFactor - 1)
  );
}

function calculateShortestPathForAllPairs(
  positions,
  emptyRowIndices,
  emptyColumnIndices,
  replaceFactor
) {
  const shortestPaths = [];

  for (let i = 0; i < positions.length; i++) {
    for (let j = i + 1; j < positions.length; j++) {
      shortestPaths.push(
        calculateShortestPath(
          positions[i],
          positions[j],
          emptyRowIndices,
          emptyColumnIndices,
          replaceFactor
        )
      );
    }
  }

  return shortestPaths;
}

function combinedShortestPathLength(shortestPaths) {
  return shortestPaths.reduce((acc, val) => acc + val, 0);
}

// Main execution

const DEBUG = false;

const fileName = process.argv[2] || "input.txt";
const map = readFileToMap(fileName);
const emptyRowIndices = getEmptyRowIndices(map);
const emptyColumnIndices = getEmptyColumnIndices(map);

const galaxyPositions = getGalaxyPositions(map);
console.log("Galaxy Positions:", galaxyPositions);

const totalShortestPath = combinedShortestPathLength(
  calculateShortestPathForAllPairs(
    galaxyPositions,
    emptyRowIndices,
    emptyColumnIndices,
    2
  )
);

console.log(
  "Total Shortest Path Length for All Pairs with 2 Replace Factor:",
  totalShortestPath
);

const totalShortestPathMillion = combinedShortestPathLength(
  calculateShortestPathForAllPairs(
    galaxyPositions,
    emptyRowIndices,
    emptyColumnIndices,
    1_000_000
  )
);

console.log(
  "Total Shortest Path Length for All Pairs with 1,000,000 Replace Factor:",
  totalShortestPathMillion
);
