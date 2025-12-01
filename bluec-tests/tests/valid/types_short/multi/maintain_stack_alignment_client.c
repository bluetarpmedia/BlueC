short add_variables(short x, short y, short z);

int main(void) {
    /* Allocate several stack variables of different sizes */
    long x = 3;
    int y = 4;
    short z = 5;

    /* Test that we can make function calls (i.e. that stack is aligned correctly) */
    return add_variables(x, y, z);
}