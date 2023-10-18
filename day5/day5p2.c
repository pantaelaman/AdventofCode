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
  char testid[64];
  memcpy(testid, argv[1], len);

  char passwd[9];
  char temp[2];

  int i = 0;
  int num_digits = 1;
  int solved = 0;
  unsigned char* hash;
  while (1) {
    sprintf(&testid[len], "%d", i);
    hash = computehash(testid, strlen(testid));

    if (memcmp(hash, &ZEROS, 2) == 0 && (unsigned char) (~hash[2] | 0x0f) == (unsigned char) 0xff) {
      int index = hash[2] & 0x0f;
      if (index < 8 && passwd[index] == 0x00) {
        printf("%1.1x\n", hash[3] >> 4);
        snprintf(temp, 2, "%1.1x", hash[3] >> 4);
        memcpy(&passwd[index], temp, 1);
        solved++;
      }
    }

    if (solved >= 8) break;       
    i++;
  }

  printf("%s\n", passwd);

  return 0;
}