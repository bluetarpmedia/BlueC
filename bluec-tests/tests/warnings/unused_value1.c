int main(void) {
    int a = 1;

    -a;
    +a;
    ~a;
    !a;
    &a;

    int *ptr = &a;
    *ptr;

    // Should emit no warnings:
    ++a;
    a++;
    --a;
    a--;

    return a;
}