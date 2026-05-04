int main(void) {
    int a = 0;

    if (a && 1) // OK
        return 0;

    if (a || 0) // OK
        return 0;

    if (a && 3 - 4)
        return 0;

    if (a || 3 + 2)
        return 0;

    return 0;
}