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
  int j = 0;
  while (1) {
    position = ftell(fptr);
    fscanf(fptr, "%*[^\n]");
    len = ftell(fptr) - position;
    fseek(fptr, position, SEEK_SET);

    bool valid = false;
    bool inhyper = false;
    char buf[2] = {0x00};
    char* line = calloc(len + 1, sizeof(char));

    fgets(line, len + 1, fptr);
    printf("%s\n", line);
    for (int i = 0; i < len; i++) {
      if (line[i] == '[') {
        inhyper = true;
        buf[0] = 0x00;
        buf[1] = 0x00;
        continue;
      }
      if (line[i] == ']') {
        inhyper = false;
        buf[0] = 0x00;
        buf[1] = 0x00;
      }
      if (buf[1] != buf[0] && line[i] == buf[1]) {
        if (line[++i] == buf[0]) {
          valid = !inhyper;
          if (inhyper) {
            break;
          } else {
            continue;
          }
        }
        else i--;
      }
      buf[0] = buf[1];
      buf[1] = line[i];
    }

    printf("** %b\n", valid);
    if (valid) count++;  

    fgetc(fptr); // discard '\n'
    char t = fgetc(fptr);
    if (t == EOF) break;
    ungetc(t, fptr);
  }

  printf("%d\n", count);
  
  return 0;
}
