#include <string.h>
#include <stdio.h>

#include "md5.h"

int numdigits(int i) {
  int count = 0;
  do {
    i /= 10;
    count++;
  } while (i != 0);

  return count;
}

short ZEROS = 0x0000;

int main(int argc, char** argv) {
  size_t len = strlen(argv[1]);
  // char* testid = calloc((len + 2), sizeof(char));
  char testid[64];
  memcpy(testid, argv[1], len);

  int i = 0;
  int num_digits = 1;
  int solved = 0;
  unsigned char* hash;
  while (1) {
    int num_digits = snprintf(&testid[len], 64-len, "%d", i);
    hash = computehash(testid, len + num_digits);

    if (memcmp(hash, &ZEROS, 2) == 0 && (unsigned char) (~hash[2] | 0x0f) == (unsigned char) 0xff) {
      printf("%1.1x", hash[2]);
      solved++;
    }

    // printf("%s\n", testid);
    if (solved >= 8) break;
    
    // int new_digits = numdigits(++i);
    // if (new_digits > num_digits) {
    //   char* new = calloc((len + num_digits + 2), sizeof(char));
    //   memcpy(new, testid, len + num_digits);
    //   free(testid);
    //   testid = new;
    //   num_digits = new_digits;
    // }

    i++;
  }

  printf("\n");

  return 0;
}