/* Test that we can access global objects of character type in multiple translation units */

extern _Bool a;
extern _Bool b;
extern _Bool c;

int flip_bools(void);

int main(void) {
    // check initial values
    if (a != 1) {
        return 1;
    }

    if (b != 0) {
        return 2;
    }

    if (c != 1) {
        return 3;
    }

    flip_bools();

    // check updated values
    if (a != 0) {
        return 4;
    }

    if (b != 1) {
        return 5;
    }

    if (c != 0) {
        return 6;
    }

    return 0;
}
