const DEBUG = false;

const NORTH = "n";
const SOUTH = "s";
const EAST = "e";
const WEST = "w";

function readFileToPlayground(filePath) {
  const fs = require("fs");
  const content = fs.readFileSync(filePath, "utf-8");
  const lines = content.split("\n").map((line) => line.trim());

  const playground = [];

  for (const line of lines) {
    playground.push(line.split(""));
  }
  return playground;
}

function getStartPositionOnPlayground(playground) {
  for (let row of playground) {
    if (row.includes("S")) {
      return { x: row.indexOf("S"), y: playground.indexOf(row) };
    }
  }

  return null;
}

function getPipeDirectionsAtPosition(playground, position) {
  switch (playground[position.y][position.x]) {
    case "|":
      return [NORTH, SOUTH];
    case "-":
      return [EAST, WEST];
    case "L":
      return [NORTH, EAST];
    case "J":
      return [NORTH, WEST];
    case "7":
      return [SOUTH, WEST];
    case "F":
      return [SOUTH, EAST];
    case ".":
      return [];
    case "S":
      return [NORTH, SOUTH, EAST, WEST];
    default:
      throw new Error(
        `Unknown pipe character ${
          playground[position.y][position.x]
        } at position (${position.x}, ${position.y})`
      );
  }
}

function moveInDirection(position, direction) {
  switch (direction) {
    case NORTH:
      return { x: position.x, y: position.y === 0 ? 0 : position.y - 1 };
    case SOUTH:
      return {
        x: position.x,
        y: position.y === playground.length - 1 ? position.y : position.y + 1,
      };
    case EAST:
      return {
        x:
          position.x === playground[0].length - 1 ? position.x : position.x + 1,
        y: position.y,
      };
    case WEST:
      return { x: position.x === 0 ? 0 : position.x - 1, y: position.y };
    default:
      throw new Error(`Unknown direction: ${direction}`);
  }
}

function invertDirection(direction) {
  switch (direction) {
    case NORTH:
      return SOUTH;
    case SOUTH:
      return NORTH;
    case EAST:
      return WEST;
    case WEST:
      return EAST;
    default:
      throw new Error(`Unknown direction: ${direction}`);
  }
}

function isNextMovePossible(playground, currentPosition, targetDirection) {
  const pipeAtNextPosition = getPipeDirectionsAtPosition(
    playground,
    moveInDirection(currentPosition, targetDirection)
  );

  return pipeAtNextPosition.includes(invertDirection(targetDirection));
}

function isStartPosition(playground, position) {
  return playground[position.y][position.x] === "S";
}

/**
 *
 * @param {*} playground
 * @param {*} currentPosition
 * @param {*} comingFromDirection The inverted position of what was the last direction moved. So if you moved NORTH, this will be SOUTH.
 * @returns
 */
function getNextDirections(playground, currentPosition, comingFromDirection) {
  const possibleDirections = getPipeDirectionsAtPosition(
    playground,
    currentPosition
  ).filter((dir) => dir !== comingFromDirection);

  return possibleDirections;
}

function findLongestLoopInPlayground(playground, startPosition) {
  const currentPosition = { ...startPosition };
  const possibleDirections = getNextDirections(playground, currentPosition, "");

  return Math.max(
    ...possibleDirections.map((dir) =>
      explore(playground, currentPosition, dir, 0)
    )
  );
}

function explore(playground, currentPosition, nextDirection, movedSteps) {
  if (DEBUG) {
    console.log(
      `At position (${currentPosition.x}, ${currentPosition.y}), moving ${nextDirection}, steps so far: ${movedSteps}`
    );
  }

  if (!isNextMovePossible(playground, currentPosition, nextDirection)) {
    return -1;
  }

  currentPosition = moveInDirection(currentPosition, nextDirection);
  movedSteps += 1;

  if (isStartPosition(playground, currentPosition)) {
    return movedSteps;
  }

  const nextDirections = getNextDirections(
    playground,
    currentPosition,
    invertDirection(nextDirection)
  );

  const paths = [];
  for (const dir of nextDirections) {
    paths.push(explore(playground, { ...currentPosition }, dir, movedSteps));
  }

  return Math.max(...paths);
}

// Main Execution

const assert = require("assert");

const fileName = process.argv[2] || "input.txt";
const playground = readFileToPlayground(fileName);
const startPosition = getStartPositionOnPlayground(playground);
if (DEBUG) {
  console.log(playground, startPosition);
}
assert(
  startPosition !== null,
  "Start position 'S' not found in the playground"
);

const longestLoopLength = findLongestLoopInPlayground(
  playground,
  startPosition
);
console.log("Longest loop length:", longestLoopLength);

const farthestDistance = Math.ceil(longestLoopLength / 2);
console.log("Farthest distance from start:", farthestDistance);
