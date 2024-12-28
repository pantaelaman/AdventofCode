#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int a;
  int b;
  int c;
} Triangle;

typedef struct {
  Triangle* triangles;
  size_t num_triangles;
} Triangles;

Triangles readtriangles(FILE *fptr) {
  Triangles triangles;

  triangles.num_triangles = 0;
  while (!feof(fptr)) {
    fscanf(fptr, "%*[^\n]\n");
    triangles.num_triangles++;
  }

  fseek(fptr, 0, SEEK_SET);

  triangles.triangles = malloc(sizeof(Triangle) * triangles.num_triangles);
  for (int i = 0; i < triangles.num_triangles / 3; i++) {
    int l[9];
    fscanf(fptr, 
      "%d %d %d\n%d %d %d\n%d %d %d\n",
      &l[0], &l[1], &l[2], &l[3], &l[4], &l[5], &l[6], &l[7], &l[8]);

    triangles.triangles[i*3] = (Triangle) {.a = l[0], .b = l[3], .c = l[6]};
    triangles.triangles[i*3 + 1] = (Triangle) {.a = l[1], .b = l[4], .c = l[7]};
    triangles.triangles[i*3 + 2] = (Triangle) {.a = l[2], .b = l[5], .c = l[8]};
  }

  return triangles;
}

int main(int argc, char** argv) {
  FILE *fptr = fopen(argv[1], "r");
  Triangles triangles = readtriangles(fptr);
  fclose(fptr);

  int possibles = 0;
  for (int i = 0; i < triangles.num_triangles; i++) {
    Triangle triangle = triangles.triangles[i];
    if (
      triangle.a + triangle.b > triangle.c &&
      triangle.a + triangle.c > triangle.b &&
      triangle.b + triangle.c > triangle.a
    ) possibles++;
  }

  printf("*** %d\n", possibles);
}
