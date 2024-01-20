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

const out = [];

for (let j = 0; j < dstHeight; j++) {
	for (let i = 0; i < dstWidth; i++) {
		const x = -1 + (2 * i) / dstWidth;
		const y = -1 + (2 * j) / dstHeight;
		const r = sqrt(x * x + y * y);
		const a = atan2(y, x);

		let x1 = x * (dstWidth / dstHeight);
		let y1 = y * aspect;
		const r1 = sqrt(x1 * x1 + y1 * y1); // Aspect corrected radius

		// Tunnel
		const uRept = 1;
		const vScale = 3;
		// let v = 1 / (r1 * vScale);
		// let u = (a * uRept) / (2 * PI);

		// Flat plane
		// u = x/(Math.sqrt((y*y))+.1)
		// v = 1/(Math.sqrt((y*y))+.1)

		// Star
		// v = 1 / (r1 + 0.5 + 0.5 * sin(2 * a));
		// u = (a * 2) / PI;

		// // Distort
		// u = r*cos(a+r)
		// v = r*sin(a+r)

		// Distort 2
		// u = 0.1*x/(0.11+r/10)
		// v = 0.1*y/(0.11+r/10)

		// const yOffs = round(srcHeight * v) % srcHeight;
		// const xOffs = round(srcWidth * u) % srcWidth;
		// let offs = yOffs * srcWidth + xOffs;
		// if (offs < 0) {
		//   offs += srcHeight * srcWidth;
		// }

		// out.push(offs)

		// Circle
		const f = ((1 - sqrt(1 - r1 * 2)) / r1) * 2;
		if (f >= 0) {
			v = (y * f) / 2.5;
			u = (x * f) / 2;

			if (v > 0x8000) {
				v = 0;
			}
		} else {
			//   out.push(-1)
			v = 1 / (r1 + 0.5 + 0.5 * sin(2 * a));
			u = (a * 2) / PI;

			u = (0.1 * x) / (0.11 + r / 10);
			v = (0.1 * y) / (0.11 + r / 10);
		}

		let yOffs = floor(srcHeight * v) % srcHeight;
		let xOffs = floor(srcWidth * u) % srcWidth;
		if (yOffs < 0) {
			yOffs += srcHeight;
		}
		if (xOffs < 0) {
			xOffs += srcWidth;
		}
		let offs = yOffs * srcWidth + xOffs;
		// if (offs < 0) {
		//   offs += srcHeight * srcWidth;
		// }
		out.push(offs);
	}
}

const blocks = [];
for (let i = 0; i < out.length / 8; i++) {
	const idx = i * 8;
	blocks.push([out[idx], out[idx + 1], out[idx + 4], out[idx + 5]]);
	blocks.push([out[idx + 2], out[idx + 3], out[idx + 6], out[idx + 7]]);
}

const opts = {
	doubleFirst: 0,
	doubleSecond: 0,
	sequential: 0,
};

blocks.forEach(([a, b, c, d], i) => {
	const n = 7 - (i % 8);
	// if (d === a+4) {
	if (d === c + 1) {
		opts.sequential++;
		console.log(` move.w ${hex(d * 2)}(a4),d${n}`);
	} else if (d === c) {
		opts.doubleFirst++;
		console.log(` move.w ${hex(d * 2)}(a3),d${n}`);
	} else {
		console.log(` move.w ${hex(d * 2)}(a1),d${n}`);
		console.log(` or.w ${hex(c * 2)}(a2),d${n}`);
	}

	if (b === a + 1) {
		opts.sequential++;
		console.log(` move.b ${hex(b * 2)}(a4),d${n}`);
	} else if (b === a) {
		opts.doubleSecond++;
		console.log(` move.b ${hex(b * 2)}(a3),d${n}`);
	} else {
		console.log(` move.b ${hex(b * 2)}(a1),d${n}`);
		console.log(` or.b ${hex(a * 2)}(a2),d${n}`);
	}

	if (n === 0) {
		console.log(` movem.w d0-d7,-(a0)`);
	}
});

console.log(" rts");

console.log(
	Object.keys(opts)
		.map((k) => "; " + k + ": " + opts[k])
		.join("\n")
);

function hex(v) {
	return "$" + v.toString(16);
}
