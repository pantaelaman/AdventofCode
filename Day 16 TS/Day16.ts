import fs from "fs";

class Packet {
    version: number;
    type: number;
    contains: Packet[] | number;

    constructor(program: boolean[]) {
        this.version = parse_programmed([program.shift()!, program.shift()!, program.shift()!]);
        this.type = parse_programmed([program.shift()!, program.shift()!, program.shift()!]);
        if (this.type == 4) {
            let counter = 0;
            let result = 0;
            while (true) {
                let buffer = program.shift();
                let value = parse_programmed([program.shift()!,program.shift()!,program.shift()!,program.shift()!]);
                result += value << (counter * 4);
                if (!buffer) {
                    break
                }
                counter++;
            }
            this.contains = result;
        } else {
            if (program.shift()!) {
                let buffer: boolean[] = [];
                for (let i = 0; i < 11; i++) {
                    buffer.push(program.shift()!);
                }
                let count = parse_programmed(buffer);
                let packets: Packet[] = [];
                for (let i = 0; i < count; i++) {
                    packets.push(new Packet(program));
                }
                this.contains = packets;
            } else {
                let buffer: boolean[] = [];
                for (let i = 0; i < 15; i++) {
                    buffer.push(program.shift()!);
                }
                let bits = parse_programmed(buffer);
                let packets: Packet[] = [];
                let orig_size = program.length;
                let current_size = program.length;
                while ((orig_size - current_size) < bits) {
                    packets.push(new Packet(program));
                    current_size = program.length;
                    console.log(`Size: ${current_size}`);
                }
                this.contains = packets;
            }
        }
    }
}

function parse_hexadecimal(hex: string): boolean[] {
    let result: boolean[][] = [];
    hex.split("").forEach((character) => {
        switch (character) {
            case "0":
                result.push([false,false,false,false]);
            case "1":
                result.push([false,false,false,true]);
            case "2":
                result.push([false,false,true,false]);
            case "3":
                result.push([false,false,true,true]);
            case "4":
                result.push([false,true,false,false]);
            case "5":
                result.push([false,true,false,true]);
            case "6":
                result.push([false,true,true,false]);
            case "7":
                result.push([false,true,true,true]);
            case "8":
                result.push([true,false,false,false]);
            case "9":
                result.push([true,false,false,true]);
            case "a":
                result.push([true,false,true,false]);
            case "b":
                result.push([true,false,true,true]);
            case "c":
                result.push([true,true,false,false]);
            case "d":
                result.push([true,true,false,true]);
            case "e":
                result.push([true,true,true,false]);
            case "f":
                result.push([true,true,true,true]);
        }
    });
    return result.flat();
}

function parse_programmed(data: boolean[]): number {
    let result: number = 0;
    let corrected = data.reverse();
    corrected.forEach((datum,index) => {
        if (datum) {
            result += 1 << index;
        }
    });
    return result;
}

let test = fs.readFileSync("Day16Test.txt", "utf-8");
let input = fs.readFileSync("Day16Input.txt", "utf-8");

let packet = new Packet(parse_hexadecimal(test.split("\n")[0]));

console.log(packet);
