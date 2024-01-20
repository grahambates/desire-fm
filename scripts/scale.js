#!/usr/bin/env node
/**
 * Generates a Lookup Table of columns to remove in order resize an image.
 */

/*
For each intermediate step
Find which pixel which if removed move pixels closest to 'ideal' value
Interpolate to get ideal values - can be float?
Total deltas for each pixel from ideal

try for each possible pixel
choose lowest total delta
*/

// Initial width of image
const start = 288;

const srcCols = []; // Absolute index of columns removed at each step in relation to original image
const colPositions = []; // Relative index from current image state

// Keep track of remaining columns at each step in order to get relative positions
const remainingCols = [];
for (let i = 0; i < start; i++) {
  remainingCols.push(i);
}

function idealPos(width) {
  const positions = [];
  const inc = start / width;
  for (let i = 0; i < width; i++) {
    positions.push(inc * i);
  }
  return positions;
}

function totalDelta(candidate, ideal) {
  let total = 0;
  for (let i = 0; i < candidate.length; i++) {
    total += Math.abs(candidate[i] - ideal[i]);
  }
  return total;
}

function findBest() {
  const ideal = idealPos(remainingCols.length - 1);
  const deltas = [];
  for (let i = 0; i < remainingCols.length; i++) {
    const candidate = remainingCols.slice();
    candidate.splice(i, 1);
    deltas.push(totalDelta(candidate, ideal));
  }
  const minDelta = Math.min(...deltas);
  const bestIdx = deltas.indexOf(minDelta);
  return bestIdx;
}

function buildLut(numbers, size = "w", perRow = 8) {
  const rows = Math.ceil(numbers.length / perRow);
  let out = "";
  for (let i = 0; i < rows; i++) {
    const rowVals = numbers.splice(0, perRow);
    out += ` dc.${size} ${rowVals.join(",")}\n`;
  }
  return out;
}

for (let i = 0; i < start; i++) {
  const idx = findBest();
  colPositions.push(idx);
  srcCols.push(remainingCols[idx]);
  remainingCols.splice(idx, 1);
}

// Left hand offset as image content is shifted into center
let offset = 0;
for (let i in colPositions) {
  colPositions[i] += offset;
  // Increase offset for columns removed on lhs
  if (colPositions[i] < start / 2) {
    offset += 1;
  }
}

colPositions.reverse();
srcCols.reverse();

console.log("ColPositions:");
console.log(buildLut(colPositions));
console.log("ColPositionsE:\n");

console.log("SrcCols:");
console.log(buildLut(srcCols));
console.log("SrcColsE:");
