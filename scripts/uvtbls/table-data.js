const { sin, cos, abs, PI, round, floor, sqrt, atan2, min, max } = Math;
const argv = require("yargs/yargs")(process.argv.slice(2)).options({
  u: {
    alias: "dstWidth",
    default: 320 / 2,
    type: "number",
  },
  v: {
    alias: "dstHeight",
    default: 220 / 2,
    type: "number",
  },
  w: {
    alias: "srcWidth",
    default: 128,
    type: "number",
  },
  h: {
    alias: "srcHeight",
    default: 128,
    type: "number",
  },
  a: {
    alias: "aspect",
    default: 1,
    type: "number",
  },
}).argv;

const { dstWidth, dstHeight, srcWidth, srcHeight, aspect } = argv;

const xOffsets = [];
const yOffsets = [];

for (let j = 0; j < dstHeight; j++) {
  const y = (2 * j) / dstWidth - dstHeight / dstWidth;
  for (let i = 0; i < dstWidth; i++) {
    const x = (2 * i) / dstWidth - 1;
    const r = sqrt(x * x + y * y);
    const a = atan2(y, x);

    // Circle
    const f = ((1 - sqrt(1 - r * 2.5)) / r) * 2;
    if (f >= 0) {
      v = (y * f) / 4;
      u = (x * f) / 4;

      if (v > 0x8000) {
        v = 0;
      }
    } else {
      u = (0.1 * x) / (0.11 + r / 14);
      v = (0.1 * y) / (0.11 + r / 14);
    }

    let yOffs = floor(srcHeight * v) % srcHeight;
    let xOffs = floor(srcWidth * u) % srcWidth;
    if (yOffs < 0) {
      yOffs += srcHeight;
    }
    if (xOffs < 0) {
      xOffs += srcWidth;
    }
    xOffsets.push(xOffs);
    yOffsets.push(yOffs);
  }
}

console.log("XDeltas:");
console.log("\t dc.b\t" + makeDeltas(xOffsets).join(","));
console.log("YDeltas:");
console.log("\t dc.b\t" + makeDeltas(yOffsets).join(","));

function makeDeltas(offsets) {
  let last = offsets[0];
  const out = [last];
  for (let i = 1; i < offsets.length; i++) {
    let delta = offsets[i] - last
    // if (delta < -64) {
    //   delta += srcWidth - 1
    // }
    out.push(delta);
    last = offsets[i];
  }
  return out;
}
