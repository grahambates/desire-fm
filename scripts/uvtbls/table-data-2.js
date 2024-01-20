const CHUNKY_W = 320 / 2;
const CHUNKY_H = 220 / 2;
const TEX_W = 128;

const deltas = [];

const W = CHUNKY_W / 2;
const H = CHUNKY_H / 2;

for (let j = 0; j < W; j++) {
	const y = j / W;
	let curr = 0;
	for (let i = 0; i < W; i++) {
		// Empty unused region to save compressed space
		if (i > H && j > H) {
			deltas.push(0);
			continue;
		}

		let u = 0;
		const x = i / W;
		const r = Math.sqrt(x * x + y * y);

		const sphereZoom = 2;
		const sphereScale = 2.7;
		const distAmount = 10;

		const f =
			((1 - Math.sqrt(1 - r * sphereScale)) / r) * sphereZoom;
		if (f >= 0) {
			// Circle
			u = (x * f) / 4;
		} else {
			// Background
			u = x / distAmount / (r / distAmount + 0.1);
		}

		let xOffs = Math.floor(TEX_W * u);
		deltas.push(xOffs - curr);
		curr = xOffs;
	}
}

console.log("Deltas:");
console.log("\t dc.b\t" + deltas.join(","));
process.exit();

// Test converting to values - need to do this in ASM

const xOffsets = [];
const yOffsets = [];

const base = CHUNKY_W / 2 + (CHUNKY_H / 2) * CHUNKY_W;
let xOffsetsAsc = base;
let xOffsetsDesc = base - CHUNKY_W;

for (let y = 0; y < CHUNKY_W / 2; y++) {
	let pos = TEX_W / 2;
	let neg = TEX_W / 2;

	let yOffsetsAsc = base;
	let yOffsetsDesc = base - CHUNKY_W;

	for (let x = 0; x < CHUNKY_W / 2; x++) {
		let delta = deltas.shift();
		pos += delta;
		neg -= delta;
		if (y < H) {
			xOffsets[xOffsetsAsc + x] = pos;
			xOffsets[xOffsetsDesc + x] = pos;
			xOffsets[xOffsetsAsc - x - 1] = neg;
			xOffsets[xOffsetsDesc - x - 1] = neg;
		}

		yOffsets[yOffsetsAsc + y] = pos;
		yOffsets[yOffsetsAsc - y - 1] = pos;
		yOffsets[yOffsetsDesc + y] = neg;
		yOffsets[yOffsetsDesc - y - 1] = neg;

		yOffsetsAsc += CHUNKY_W;
		yOffsetsDesc -= CHUNKY_W;
	}
	xOffsetsAsc += CHUNKY_W;
	xOffsetsDesc -= CHUNKY_W;
}

console.log("XDeltas:");
console.log("\t dc.b\t" + xOffsets.join(","));
console.log("YDeltas:");
console.log("\t dc.b\t" + yOffsets.join(","));
