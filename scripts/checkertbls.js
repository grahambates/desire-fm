const steps = 64;
const a = Math.PI / steps;
const tan = [];
const sin = [];
for (let i = 0; i < steps; i++) {
        const ang = a * (i + 0.5);
        tan.push(Math.tan(ang));
        sin.push(1 / Math.sin(ang));
}

console.log(
        formatTable(
                tan.map((v) => Math.round((1 / v) * (1 << 16))),
                { label: "TanR" }
        )
);

console.log(
        formatTable(
                tan.map((v) => Math.round(v * (1 << 8))),
                { label: "Tan", size: "w" }
        )
);

console.log(
        formatTable(
                sin.map((v) => Math.round(v * (1 << 8))),
                { label: "SinR", size: "w" }
        )
);

function formatTable(values, options = {}) {
        const opts = {
                size: "l",
                rowSize: 16,
                hex: true,
                ...options,
        };
        let output = opts.label ? opts.label + ":" : "";
        for (let i in values) {
                output += i % opts.rowSize ? "," : `\n  dc.${opts.size}  `;
                output += opts.hex
                        ? formatHex(values[i], opts.size)
                        : values[i];
        }
        return output;
}

function formatHex(value, size) {
        const sizes = {
                b: 1,
                w: 2,
                l: 4,
        };
        const l = sizes[size];
        const max = Math.pow(2, 8 * l);
        if (value < 0) {
                value = max + value;
        }
        value = value % max;
        return "$" + value.toString(16).padStart(l * 2, "0");
}
