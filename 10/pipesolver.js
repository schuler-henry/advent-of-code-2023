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
    case "H":
    case "O":
    case "U":
    case "V":
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
      if (position.y === 0) {
        throw new Error("Cannot move north");
      }
      return { x: position.x, y: position.y - 1 };
    case SOUTH:
      if (position.y === playground.length - 1) {
        throw new Error("Cannot move south");
      }
      return {
        x: position.x,
        y: position.y + 1,
      };
    case EAST:
      if (position.x === playground[0].length - 1) {
        throw new Error("Cannot move east");
      }
      return {
        x: position.x + 1,
        y: position.y,
      };
    case WEST:
      if (position.x === 0) {
        throw new Error("Cannot move west");
      }
      return { x: position.x - 1, y: position.y };
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
  let nextPosition = null;
  try {
    nextPosition = moveInDirection(currentPosition, targetDirection);
  } catch (error) {
    return false;
  }
  const pipeAtNextPosition = getPipeDirectionsAtPosition(
    playground,
    nextPosition
  );

  return pipeAtNextPosition.includes(invertDirection(targetDirection));
}

function isMarkedPosition(playground, position) {
  return ["S", "H", "O", "U", "V"].includes(playground[position.y][position.x]);
}

function markPosition(playground, position, directions) {
  if (!directions) {
    directions = getNextDirections(playground, position, "");
  }
  if (directions.includes(NORTH) && directions.includes(SOUTH)) {
    playground[position.y][position.x] = "V";
  } else if (directions.includes(NORTH)) {
    playground[position.y][position.x] = "O";
  } else if (directions.includes(SOUTH)) {
    playground[position.y][position.x] = "U";
  } else {
    playground[position.y][position.x] = "H";
  }
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

function findLoopInPlayground(playground, startPosition) {
  const currentPosition = { ...startPosition };
  const possibleDirections = getNextDirections(playground, currentPosition, "");

  for (let dir of possibleDirections) {
    const markedPlayground = JSON.parse(JSON.stringify(playground));
    const loop = explore(markedPlayground, currentPosition, dir, 0);

    if (loop !== null) {
      return {
        direction: dir,
        size: loop.size,
        markedPlayground: markedPlayground,
      };
    }
  }

  throw new Error("No loop found");
}

function explore(playground, currentPosition, nextDirection, movedSteps) {
  if (DEBUG) {
    console.log(
      `At position (${currentPosition.x}, ${currentPosition.y}), moving ${nextDirection}, steps so far: ${movedSteps}`
    );
  }

  const startDirectionA = nextDirection;
  let startDirectionB = null;

  while (movedSteps === 0 || !isMarkedPosition(playground, currentPosition)) {
    if (!isNextMovePossible(playground, currentPosition, nextDirection)) {
      return null;
    }

    markPosition(playground, currentPosition);
    currentPosition = moveInDirection(currentPosition, nextDirection);
    movedSteps += 1;

    startDirectionB = invertDirection(nextDirection);
    nextDirection = getNextDirections(
      playground,
      currentPosition,
      invertDirection(nextDirection)
    )[0];
  }

  if (DEBUG) {
    console.log(
      "Directions for start node: ",
      startDirectionA,
      startDirectionB
    );
  }
  markPosition(playground, currentPosition, [startDirectionA, startDirectionB]);

  return {
    size: movedSteps,
  };
}

function calculateAreaOfLoop(markedPlayground) {
  const OUTSIDE = "O";
  const INSIDE = "I";

  let area = 0;
  for (let row of markedPlayground) {
    let state = OUTSIDE;

    for (let cell of row) {
      switch ([state, cell].toString()) {
        case [OUTSIDE, "V"].toString():
          state = INSIDE;
          break;
        case [INSIDE, "V"].toString():
          state = OUTSIDE;
          break;
        case [OUTSIDE, "U"].toString():
          state = OUTSIDE + "U";
          break;
        case [OUTSIDE, "O"].toString():
          state = OUTSIDE + "O";
          break;
        case [OUTSIDE + "U", "U"].toString():
        case [OUTSIDE + "O", "O"].toString():
          state = OUTSIDE;
          break;
        case [OUTSIDE + "U", "O"].toString():
        case [OUTSIDE + "O", "U"].toString():
          state = INSIDE;
          break;
        case [INSIDE, "U"].toString():
          state = INSIDE + "U";
          break;
        case [INSIDE, "O"].toString():
          state = INSIDE + "O";
          break;
        case [INSIDE + "U", "U"].toString():
        case [INSIDE + "O", "O"].toString():
          state = INSIDE;
          break;
        case [INSIDE + "U", "O"].toString():
        case [INSIDE + "O", "U"].toString():
          state = OUTSIDE;
          break;
        default:
          if (cell === "H") {
            break;
          }
          if (state === INSIDE) {
            area += 1;
          }
      }
    }
  }

  return area;
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

const longestLoop = findLoopInPlayground(playground, startPosition);
console.log("Longest loop length:", longestLoop.size);

const farthestDistance = Math.ceil(longestLoop.size / 2);
console.log("Farthest distance from start:", farthestDistance);

if (DEBUG) {
  console.log(longestLoop.markedPlayground);
}

const areaOfLoop = calculateAreaOfLoop(longestLoop.markedPlayground);
console.log("Area of the loop:", areaOfLoop);
