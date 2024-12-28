#include <stdio.h>
#include <stdlib.h>

const char keypad[25] = {
  0x0, 0x0, '1', 0x0, 0x0,
  0x0, '2', '3', '4', 0x0,
  '5', '6', '7', '8', '9',
  0x0, 'A', 'B', 'C', 0x0,
  0x0, 0x0, 'D', 0x0, 0x0,
};

typedef enum {L, R, U, D} Direction;

typedef struct {
  Direction** directions;
  size_t* counts;
  size_t num_inputs;
} Directions;

Directions readdirections(FILE *fptr) {
  Directions directions;
  
  directions.num_inputs = 0;
  while (!feof(fptr)) {
    fscanf(fptr, "%*[^\n]\n");
    directions.num_inputs++;
  }

  fseek(fptr, 0, SEEK_SET);

  directions.directions = malloc(sizeof(Direction*) * directions.num_inputs);
  directions.counts = malloc(sizeof(size_t) * directions.num_inputs);
  int mark;
  for (int i = 0; i < directions.num_inputs; i++) {
    mark = ftell(fptr);
    fscanf(fptr, "%*[^\n]\n");
    int count = ftell(fptr) - mark - 1;
    directions.counts[i] = count;
    fseek(fptr, mark, SEEK_SET);

    directions.directions[i] = malloc(sizeof(Direction) * count);
    for (int j = 0; j < count; j++) {
      switch (fgetc(fptr)) {
        case 'L':
          directions.directions[i][j] = L;
          break;
        case 'R':
          directions.directions[i][j] = R;
          break;
        case 'U':
          directions.directions[i][j] = U;
          break;
        case 'D':
          directions.directions[i][j] = D;
      }
    }

    fgetc(fptr);
  }

  return directions;
}

int min(int a, int b) {
  return a > b ? b : a;
}

int max(int a, int b) {
  return a < b ? b : a;
}

int sclamp(int cross, int ref) {
  int bound = abs(ref - 2);
  if (cross < bound)
    return bound;
  else if (cross > 4-bound)
    return 4-bound;
  else
    return cross;
}

int main(int argc, char** argv) {
  FILE *fptr = fopen(argv[1], "r");

  Directions directions = readdirections(fptr);

  char* code = malloc(sizeof(char) * directions.num_inputs);
  int position_x = 0;
  int position_y = 2;
  for (int i = 0; i < directions.num_inputs; i++) {
    for (int j = 0; j < directions.counts[i]; j++) {
      switch (directions.directions[i][j]) {
        case L:
          position_x = sclamp(position_x - 1, position_y);
          break;
        case R:
          position_x = sclamp(position_x + 1, position_y);
          break;
        case U:
          position_y = sclamp(position_y - 1, position_x);
          break;
        case D:
          position_y = sclamp(position_y + 1, position_x);
      }
      printf("    %d:%d | %d\n", position_x, position_y, directions.directions[i][j]);
    }
    printf("%d:%d\n", position_x, position_y);
    code[i] = keypad[position_y*5 + position_x];
  }

  printf("%s\n", code);
}
