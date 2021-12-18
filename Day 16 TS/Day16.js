"use strict";
exports.__esModule = true;
var fs_1 = require("fs");
var Packet = /** @class */ (function () {
    function Packet(program) {
        this.version = parse_programmed([program.shift(), program.shift(), program.shift()]);
        this.type = parse_programmed([program.shift(), program.shift(), program.shift()]);
        if (this.type == 4) {
            var counter = 0;
            var result = 0;
            while (true) {
                var buffer = program.shift();
                var value = parse_programmed([program.shift(), program.shift(), program.shift(), program.shift()]);
                result += value << (counter * 4);
                if (!buffer) {
                    break;
                }
                counter++;
            }
            this.contains = result;
        }
        else {
            if (program.shift()) {
                var buffer = [];
                for (var i = 0; i < 11; i++) {
                    buffer.push(program.shift());
                }
                var count = parse_programmed(buffer);
                var packets = [];
                for (var i = 0; i < count; i++) {
                    packets.push(new Packet(program));
                }
                this.contains = packets;
            }
            else {
                var buffer = [];
                for (var i = 0; i < 15; i++) {
                    buffer.push(program.shift());
                }
                var bits = parse_programmed(buffer);
                var packets = [];
                var orig_size = program.length;
                var current_size = program.length;
                while ((orig_size - current_size) != bits) {
                    packets.push(new Packet(program));
                    current_size = program.length;
                }
                this.contains = packets;
            }
        }
    }
    return Packet;
}());
function parse_hexadecimal(hex) {
    var result = [];
    hex.split("").forEach(function (character) {
        switch (character) {
            case "0":
                result.push([false, false, false, false]);
            case "1":
                result.push([false, false, false, true]);
            case "2":
                result.push([false, false, true, false]);
            case "3":
                result.push([false, false, true, true]);
            case "4":
                result.push([false, true, false, false]);
            case "5":
                result.push([false, true, false, true]);
            case "6":
                result.push([false, true, true, false]);
            case "7":
                result.push([false, true, true, true]);
            case "8":
                result.push([true, false, false, false]);
            case "9":
                result.push([true, false, false, true]);
            case "a":
                result.push([true, false, true, false]);
            case "b":
                result.push([true, false, true, true]);
            case "c":
                result.push([true, true, false, false]);
            case "d":
                result.push([true, true, false, true]);
            case "e":
                result.push([true, true, true, false]);
            case "f":
                result.push([true, true, true, true]);
        }
    });
    return result.flat();
}
function parse_programmed(data) {
    var result = 0;
    var corrected = data.reverse();
    corrected.forEach(function (datum, index) {
        if (datum) {
            result += 1 << index;
        }
    });
    return result;
}
var test = fs_1["default"].readFileSync("Day16Test.txt", "utf-8");
var input = fs_1["default"].readFileSync("Day16Input.txt", "utf-8");
var packet = new Packet(parse_hexadecimal(test.split("\n")[0]));
console.log(packet);
