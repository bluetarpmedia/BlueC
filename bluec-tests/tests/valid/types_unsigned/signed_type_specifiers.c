/* Test out different ways to declare a signed int or long */

#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wold-style-declaration"
#endif
#endif

static int i;
signed extern i;
int static signed i = 5;
signed int static i;

long signed l;
long l = 7;
int long l;
signed long int l;

short signed s;
short s = 6;
int short s;
signed short int s;

int main(void) {
    int signed extern i;
    extern signed long l;
    extern signed short s;

    if (i != 5) {
        return 1;
    }

    if (l != 7) {
        return 2;
    }

    if (s != 6) {
        return 3;
    }

    /* use signed type specifier in for loop */
    int counter = 0;
    for (signed int index = 10; index > 0; index = index - 1) {
        counter = counter + 1;
    }

    if (counter != 10) {
        return 4;
    }

    return 0;
}