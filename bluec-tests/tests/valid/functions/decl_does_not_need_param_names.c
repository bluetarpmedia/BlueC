int add(int, int);

int add(int, int);

int add(int a, int b) {
    return a + b;
}

static long sub(long, long);

static long sub(long a, long b) {
    return a - b;
}

int main(void) {
    extern int mul(int, int);
    return add(1, 2) + (int)sub(10, 5) + mul(1, 2);
}

int mul(int a, int b) {
    return a * b;
}