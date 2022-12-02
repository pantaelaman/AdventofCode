import { readFileSync } from 'fs';

const contents: string = readFileSync("input.txt", "utf-8").toString();

let max_calories: [number, number, number] = [0 , 0, 0];
let current_total = 0;

contents.split('\n').forEach((line) => {
  if (line == "") {
    max_calories = update_max(current_total, max_calories);
    current_total = 0;
  } else {
    current_total += +line;
  }
});

max_calories = update_max(current_total, max_calories); // catch the last group without a newline

console.log(`Part 1: ${max_calories[0]}`);
console.log(`Part 2: ${max_calories[0] + max_calories[1] + max_calories[2]}`);

function update_max(total: number, max_calories: [number, number, number]) {
  if (total > max_calories[0]) {
    max_calories[2] = max_calories[1];
    max_calories[1] = max_calories[0];
    max_calories[0] = total;
  } else if (total > max_calories[1]) {
    max_calories[2] = max_calories[1];
    max_calories[1] = total;
  } else if (total > max_calories[2]) {
    max_calories[2] = total;
  }
  
  return max_calories;
}

