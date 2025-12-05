int square(int x) {
    return x * x;
}

int main(void) {
    int v = (&square)(5);

    if (v != 25) {
        return 1;
    }

    return 0;
}