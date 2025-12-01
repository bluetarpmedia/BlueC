int next(void) {
    static static static int value = 0;
    return ++value;
}

static static int global = 0;

int main(void) {
    extern extern int global;
    global = next();
    global = next();

    if (global != 2) {
        return 1;
    }

    return 0;
}
