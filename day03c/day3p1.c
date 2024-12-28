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
  for (int i = 0; i < triangles.num_triangles; i++) {
    int a;
    int b;
    int c;
    fscanf(fptr, "%d %d %d\n", &a, &b, &c);

    triangles.triangles[i] = (Triangle) {.a = a, .b = b, .c = c};
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
