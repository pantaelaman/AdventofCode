#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "llist.h"

typedef enum {NORTH, EAST, SOUTH, WEST} Cardinal;

Cardinal turnright(Cardinal cardinal) {
    return (cardinal + 1) % 4;
}

Cardinal turnleft(Cardinal cardinal) {
    return (cardinal - 1) % 4;
}

typedef enum {VERT, HORI} Orientation;

typedef struct {
    Cardinal dir;
    int amount;
} Command;

typedef struct {
    Command* commands;
    size_t num_commands;
} Commands;

typedef struct {
    int start;
    int end;
    int anchor;
    Orientation orientation;
} Line;

int lintersect(Line* const l1, Line* const l2) {
    if (l1->orientation == l2->orientation)
        return
            l1->anchor == l2->anchor && l1->start <= l2->end && l1->end >= l2->start;
    else
        return 
            l1->anchor >= l2->start && l1->anchor <= l2->end &&
            l2->anchor >= l1->start && l2->anchor <= l1->end;
}

char fpeek(FILE *fptr) {
    char c = fgetc(fptr);
    ungetc(c, fptr);
    return c;
}

Commands readcommands(FILE *fptr) {
    Commands commands;
    commands.num_commands = 0;
    while (fpeek(fptr) != '\n') {
        fscanf(fptr, "%*c%*d, ");
        commands.num_commands++;
    }

    fseek(fptr, 0, SEEK_SET);

    commands.commands = malloc(sizeof(Command) * commands.num_commands);
    Cardinal dir = NORTH;
    for (int i = 0; i < commands.num_commands; i++) {
        char cdir;
        int amount;
        fscanf(fptr, "%c%d, ", &cdir, &amount);
        switch (cdir) {
            case 'L':
                dir = turnleft(dir);
                break;
            case 'R':
                dir = turnright(dir);
        }
        commands.commands[i] = (Command) {.dir = dir, .amount = amount};
    }

    return commands;
}

int main(int argc, char** argv) {
    FILE *fptr = fopen(argv[1], "r");

    Commands commands = readcommands(fptr);

    int x = 0;
    int y = 0;
    LList lines= llistnew();
    int solution_x = 0;
    int solution_y = 01;
    int solved = 0;
    printf("here\n");
    for (int i = 0; i < commands.num_commands; i++) {
        Command command = commands.commands[i];
        int x_delta = 0;
        int y_delta = 0;
        switch (command.dir) {
            case NORTH:
                y_delta = command.amount;
                break;
            case EAST:
                x_delta = command.amount;
                break;
            case SOUTH:
                y_delta = -command.amount;
                break;
            case WEST:
                x_delta = -command.amount;
        }
        Line* line = malloc(sizeof(Line));
        if (x_delta == 0)
            *line = (Line) {
                .start = y_delta > 0 ? y : y + y_delta,
                .end = y_delta > 0 ? y + y_delta : y,
                .anchor = x, .orientation = HORI
            };
        else
            *line = (Line) {
                .start = x_delta > 0 ? x : x + x_delta,
                .end = x_delta > 0 ? x + x_delta : x,
                .anchor = y, .orientation = VERT
            };

        printf("%d:%d@%d|%d\n", line->start, line->end, line->anchor, line->orientation);
        for (size_t i = 1; i < lines.length; i++) {
            Line* testline = llistget(&lines, i - 1);
            printf("    %d:%d@%d|%d", testline->start, testline->end, testline->anchor, testline->orientation);
            if (lintersect(line, testline)) {
                printf(" ** ");
                int intersection_x;
                int intersection_y;
                if (line->orientation == testline->orientation) {
                    switch (line->orientation) {
                        case VERT:
                            if (y_delta > 0) {
                                intersection_x = line->anchor;
                                intersection_y = testline->end;
                            } else {
                                intersection_x = line->anchor;
                                intersection_y = testline->start;
                            }
                            break;
                        case HORI:
                            if (x_delta > 0) {
                                intersection_y = line->anchor;
                                intersection_x = testline->start;
                            } else {
                                intersection_y = line->anchor;
                                intersection_x = testline->end;
                            }
                    }
                } else {
                    switch (line->orientation) {
                        case VERT:
                            intersection_x = line->anchor;
                            intersection_y = testline->anchor;
                            break;
                        case HORI:
                            intersection_x = testline->anchor;
                            intersection_y = line->anchor;
                    }
                }

                printf("%d:%d", intersection_x, intersection_y);
                
                if (!solved) {
                    solved = 1;
                    solution_x = intersection_x;
                    solution_y = intersection_y;
                    printf("\n");
                    continue;
                }
                if (
                    (command.dir == NORTH && solution_y > intersection_y) ||
                    (command.dir == SOUTH && solution_y < intersection_y) ||
                    (command.dir == EAST  && solution_x > intersection_x) ||
                    (command.dir == WEST  && solution_x < intersection_x)
                ) {
                    solution_x = intersection_x;
                    solution_y = intersection_y;
                }
            }
            printf("\n");
        }
        if (solved) break;
        llistpush(&lines, line);
        x += x_delta;
        y += y_delta;
    }

    printf("*** %d\n", abs(solution_x) + abs(solution_y));
}
