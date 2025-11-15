// Solve part 2

const fs = require("fs");
const assert = require("assert");

const fileName = process.argv[2] || "input.txt";

const file = fs.readFileSync(fileName, "utf-8");

const lines = file.split("\n");

const instructions = lines[0].trim();
assert(instructions.length > 0, "No instructions found");
assert(instructions.match(/^[LR]+$/), "Invalid instructions format");

const dictionary = {};

let startNodes = [];

for (const line of lines.slice(2)) {
  const [key, value] = line.split(" = ");
  const [left, right] = value.replace("(", "").replace(")", "").split(", ");
  dictionary[key] = { left, right };

  if (key.endsWith("A")) {
    startNodes.push(key);
  }
}

function bruteForce(instructions, dictionary, startNodes) {
  let steps = 0;

  while (!startNodes.every((elem) => elem.endsWith("Z"))) {
    const instruction = instructions[steps % instructions.length];
    assert(instruction === "L" || instruction === "R", "Invalid instruction");
    // console.debug("Current Nodes: ", startNodes);
    // console.debug("Instruction: ", instruction);

    const nextNodes = [];

    for (node of startNodes) {
      const nextNode = dictionary[node][instruction === "L" ? "left" : "right"];
      nextNodes.push(nextNode);
    }

    // const nextNodesWithEndingZ = nextNodes.filter((elem) =>
    //   elem.endsWith("Z")
    // ).length;
    // if (nextNodesWithEndingZ > 2) {
    //   console.debug("Next Nodes with ending Z: ", nextNodesWithEndingZ);
    // }

    startNodes = nextNodes;

    steps += 1;

    if (steps % 100000000 === 0) {
      console.log("Processing: ", steps, startNodes);
    }
  }

  console.log("Steps: ", steps);
}

function analyzePattern(instructions, dictionary, startNodes) {
  console.log("Instructions length: ", instructions.length);
  const patterns = {};
  for (const startNode of startNodes) {
    let currentNode = startNode;
    patterns[startNode] = [];

    for (let step = 0; step < instructions.length * 281; step++) {
      const instruction = instructions[step % instructions.length];
      currentNode =
        dictionary[currentNode][instruction === "L" ? "left" : "right"];
      if (currentNode.endsWith("Z")) {
        patterns[startNode].push(step + 1);
        console.log((step + 1) % 281);
      }
    }
  }
  console.log("Patterns found: ", patterns);
}

// analyzePattern(instructions, dictionary, startNodes);
// => All patterns are multiples of 281 = length of instructions => find cycle length for all and get lcm

function lcm(a, b) {
  return (a * b) / gcd(a, b);
}

function gcd(a, b) {
  if (b === 0) {
    return a;
  }
  return gcd(b, a % b);
}

function computeFinalSteps(instructions, dictionary, startNodes) {
  const occurrences = [];
  for (const startNode of startNodes) {
    let currentNode = startNode;
    let steps = 0;
    while (!currentNode.endsWith("Z")) {
      const instruction = instructions[steps % instructions.length];
      currentNode =
        dictionary[currentNode][instruction === "L" ? "left" : "right"];
      steps += 1;
    }
    occurrences.push(steps);
  }

  console.log("Occurrences: ", occurrences);

  let finalLcm = occurrences[0];
  for (let i = 1; i < occurrences.length; i++) {
    finalLcm = lcm(finalLcm, occurrences[i]);
  }

  return finalLcm;
}

console.log(computeFinalSteps(instructions, dictionary, startNodes));

// 5290000000 [ 'VPT', 'JRX', 'XDC', 'JVP', 'PJJ', 'KMP' ]
// 8300000000 [ 'TSC', 'XMG', 'GRN', 'XHH', 'LQM', 'BRV' ]
