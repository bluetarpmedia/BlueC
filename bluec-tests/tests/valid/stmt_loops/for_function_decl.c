int main(void) {
    int x = 0;

    // Declare a function in the for loop's initializer expression.
    for (extern int incr(void); incr(); ) {
        x++;
    }

    if (x != 4)
        return 1;

    return 0;
}

int incr(void) {
    static int counter = 0;
    counter++;
    return counter < 5;
}