#!/usr/bin/env node

import fs from "fs";
import { ZCurve } from "@thi.ng/morton";

const MAX_VERTICES = 750;

const inputFile = process.argv[2];
const SCALE = process.argv[3];
const mirror = Boolean(process.argv[4]);

if (!inputFile) {
        console.log("Usage obj2asm.mjs <input> <scale>");
        console.log("e.g. obj2asm.mjs example.obj 100");
        process.exit(1);
}

if (!fs.statSync(inputFile)) {
        console.log(`File ${inputFile} not found`);
        process.exit(1);
}

async function init() {
        const vertices = parseObj(inputFile);
        const scaled = scale(vertices);
        const selected = shuffle(scaled).slice(
                0,
                MAX_VERTICES * (mirror ? 1 : 2)
        );
        const sorted = sort(selected);

        const [xDeltas, yDeltas, zDeltas] = getDeltas(sorted);

        printDeltas(xDeltas);
        printDeltas(yDeltas);
        printDeltas(zDeltas);
}

init();

function shuffle(input) {
        let currentIndex = input.length;
        const output = input.slice();

        while (currentIndex > 0) {
                // Pick a remaining element.
                let randomIndex = Math.floor(Math.random() * currentIndex);
                currentIndex--;

                // And swap it with the current element.
                [output[currentIndex], output[randomIndex]] = [
                        output[randomIndex],
                        output[currentIndex],
                ];
        }

        return output;
}

function parseObj(inputFile) {
        const obj = fs.readFileSync(inputFile);
        const lines = obj.toString().split("\n");
        const vertices = [];

        for (const line of lines) {
                const parts = line.split(" ").filter(Boolean);
                switch (parts[0]) {
                        case "v": {
                                const verts = parts.slice(1).map(parseFloat)
                                verts[1] = -verts[1]; // flip Y
                                vertices.push(verts);
                                break;
                        }
                }
        }

        return vertices;
}

function scale(vertices) {
        let max = 0;
        for (let v of vertices) {
                for (let p of v) {
                        max = Math.max(max, Math.abs(p));
                }
        }
        const scale = SCALE / max;

        return vertices.map((components) =>
                components.map((c) => Math.round(c * scale))
        );
}

function sort(vertices) {
        const zCurve = new ZCurve(3, 8);
        return vertices.sort((a, b) =>
                zCurve.encode(a) > zCurve.encode(b) ? 1 : -1
        );
}

function delta(v1, v2) {
        let d = v1 - v2;
        if (d < -127) d += 256;
        return d;
}

function getDeltas(vertices) {
        const xDeltas = [];
        const yDeltas = [];
        const zDeltas = [];
        let last = [0, 0, 0];
        for (let i = 0; i < vertices.length; i++) {
                xDeltas.push(delta(vertices[i][0], last[0]));
                yDeltas.push(delta(vertices[i][1], last[1]));
                zDeltas.push(delta(vertices[i][2], last[2]));
                last = vertices[i];
        }
        return [xDeltas, yDeltas, zDeltas];
}

function printDeltas(deltas) {
        console.log(`    dc.b ${deltas.join(",")}`);
}
