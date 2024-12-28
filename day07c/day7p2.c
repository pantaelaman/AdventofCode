#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool haspattern(char* line) {
  char buf[2] = {line[0], line[1]};
  for (int i = 2; line[i] != 0x00; i++) {
    if (buf[1] != buf[0] && line[i] == buf[1]) {
      if (line[++i] == buf[0])
        return true;
      else i--;
    }
    buf[0] = buf[1];
    buf[1] = line[i];
  }
  return false;
}

int main(int argc, char** argv) {
  FILE *fptr = fopen(argv[1], "r");

  size_t position;
  size_t len;
  int count = 0;
  while (1) {
    position = ftell(fptr);
    fscanf(fptr, "%*[^\n]");
    len = ftell(fptr) - position;
    fseek(fptr, position, SEEK_SET);

    bool inhyper = false;
    char buf[2] = {0x00};
    char* line = calloc(len + 1, sizeof(char));
    int numchunks = 1;
    for (int i = 0; i < len; i++) {
      char c = fgetc(fptr);
      if (c == '[' || c == ']') {
        line[i] = 0x00;
        numchunks++;
      } else line[i] = c;
    }

    char** chunks = malloc(sizeof(char*) * numchunks);
    chunks[0] = &line[0];
    char** head = chunks;
    for (int i = 0; i < len; i++) {
      char c = line[i];
      if (c == 0x00) {
        *++head = &line[i+1];
      }
    }

    printf("\n\n");

    bool solved = false;
    for (int i = 0; i < numchunks && !solved; i+=2) {
      char buf = chunks[i][0];
      printf("%s\n", chunks[i]);
      for (int j = 1; chunks[i][j] != 0x00 && !solved; j++) {
        if (chunks[i][j] != buf) {
          if (chunks[i][j+1] == buf) {
            printf("  %c%c%c\n", chunks[i][j], buf, chunks[i][j]);
            for (int k = 1; k < numchunks && !solved; k+=2) {
              for (int l = 2; chunks[k][l] != 0x00 && !solved; l++) {
                printf("    %d: %c%c%c\n", l, chunks[k][l-2], chunks[k][l-1], chunks[k][l]);
                solved = 
                  chunks[k][l-2] == chunks[i][j] &&
                  chunks[k][l-1] == buf &&
                  chunks[k][l] == chunks[i][j];
              }
            }
          }
        }
      }
    }

    printf("** %b\n", solved);
    if (solved) count++;

    fgetc(fptr); // discard '\n'
    char t = fgetc(fptr);
    if (t == EOF) break;
    ungetc(t, fptr);

    free(line);
  }

  printf("%d\n", count);
  
  return 0;
}
