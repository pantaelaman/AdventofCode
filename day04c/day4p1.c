#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

void rshiftc(char* arr, int start, int length) {
  for (int i = length - 1; i > start; i--)
    arr[i] = arr[i - 1];
}

void rshifti(int* arr, int start, int length) {
  for (int i = length - 1; i > start; i--)
    arr[i] = arr[i - 1];
}

void printcommons(char* commons, int* common_freqs) {
  for (int i = 0; i < 5; i++)
    printf("%c:%d ", commons[i], common_freqs[i]);
  printf("\n");
}

void getcommons(char* commons, int* common_freqs, int* const freq) {
  int last = 0;
  for (int i = 25; i >= 0; i--) {
    // printf("%d:%c|%d\n", i, i + 0x61, freq[i]);
    for (int j = 0; j < 5; j++) {
      if (freq[i] >= common_freqs[j]) {
        // printf("    ** accepted\n");
        rshifti(common_freqs, j, 5);
        rshiftc(commons, j, 5);
        common_freqs[j] = freq[i];
        commons[j] = i + 0x61;
        break;
      }
    }
  }
}

int main(int argc, char** argv) {
  FILE *fptr = fopen(argv[1], "r");

  int sum = 0;
  int freq[26];
  char checksum[6];
  char commons[6] = {0};
  int common_freqs[5];
  while (!feof(fptr)) {     
    // reset the frequencies and commons
    for (int i = 0; i < 26; i++) {
      freq[i] = 0;
      if (i < 5) common_freqs[i] = 0;
    }

    while (1) {
      char c = fgetc(fptr);
      if (c >= 'a' && c <= 'z')
        freq[c - 'a']++;
      else if (c >= '0' && c <= '9') {
        ungetc(c, fptr);
        int id;
        fscanf(fptr, "%d[%s\n", &id, checksum);
        checksum[5] = 0x00; // null-termination
        getcommons(commons, common_freqs, freq);
        printf("%s ; %s\n", commons, checksum);
        if (strncmp(commons, checksum, 5) == 0) {
          sum += id;
        }
        break;
      }
    }
  }

  printf("*** %d\n", sum);
  
  return 0;
}
