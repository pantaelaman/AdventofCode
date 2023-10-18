#include <stdio.h>
#include <stdlib.h>

char getcommon(int* freqs) {
  int freq = 0;
  char c;
  for (int i = 0; i < 26; i++) {
    if (freqs[i] > freq) {
      freq = freqs[i];
      c = i + 0x61;
    }
  }
  return c;
}

int main(int argc, char** argv) {
  FILE *fptr = fopen(argv[1], "r");

  fscanf(fptr, "%*[^\n]\n");
  size_t numcols = ftell(fptr) - 1; // - '\n'
  fseek(fptr, 0, SEEK_SET);

  int** freqs = calloc(numcols, sizeof(int*));
  for (int i = 0; i < numcols; i++)
    freqs[i] = calloc(26, sizeof(int));


  while (1) {
    for (int i = 0; i < numcols; i++) {
      freqs[i][fgetc(fptr) - 0x61]++;
    }
    fgetc(fptr); // discard '\n'
    char c = fgetc(fptr);
    if (c == EOF) break;
    ungetc(c, fptr);
  }

  char* msg = malloc(numcols * sizeof(char));
  for (int i = 0; i < numcols; i++) {
    msg[i] = getcommon(freqs[i]);
  }

  printf("%s\n", msg);
}
