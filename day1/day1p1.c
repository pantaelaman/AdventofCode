#include <stdio.h>
#include <stdlib.h>

typedef enum {NORTH, EAST, SOUTH, WEST} Cardinal;

Cardinal turnright(Cardinal cardinal) {
    return (cardinal + 1) % 4;
}

Cardinal turnleft(Cardinal cardinal) {
    return (cardinal - 1) % 4;
}

typedef struct {
    Cardinal dir;
    int amount;
} Command;

typedef struct {
    Command* commands;
    size_t num_commands;
} Commands;

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
    int moves[4] = {0};

    for (int i = 0; i < commands.num_commands; i++) {
        Command command = commands.commands[i];
        printf("%d:%d\n", command.dir, command.amount);
        moves[command.dir] += command.amount;
    }

    printf("%d\n", abs(moves[0] - moves[2]) + (moves[1] - moves[3]));
}
