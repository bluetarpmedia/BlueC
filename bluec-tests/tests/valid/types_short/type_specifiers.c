/* Test out different, equivalent ways to declare the same identifier  */

/* These declarations all look slightly different,
 * but they all declare 'a' as a static short, so they don't conflict.
 */
static int short a;
int static short a;
short static a;

/* These declarations all look slightly different,
 * but they all declare 'my_function' as a function
 * with three long parameters and an int return value,
 * so they don't conflict.
 */
int my_function(short a, short int b, int short c);
int my_function(short int x, int short y, short z) {
    return x + y + z;
}

int main(void) {
    /* Several different ways to declare local long variables */
    short x = 1l;
    short int y = 2l;
    int short z = 3l;

    /* This links to the file-scope declarations of 'a' above */
    extern short a;
    a = 4;

    /* make sure we can use long type specifier in for loop initializer
     * i is 2^40 so this loop should have 41 iterations
    */
    int sum = 0;
    for (short i = 100; i > 0; i = i / 2) {
        sum = sum + 1;
    }

    /* Make sure everything has the expected value */
    if (x != 1) {
        return 1;
    }

    if (y != 2) {
        return 2;
    }

    if (a != 4) {
        return 3;
    }

    if (my_function(x,  y, z) != 6) {
        return 4;
    }

    if (sum != 7) {
        return 5;
    }

    return 0;
}