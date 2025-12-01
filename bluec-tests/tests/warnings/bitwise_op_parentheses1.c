int main(void) {
    int a = 1, b = 2, c = 3;

    if (a ^ b | c) {
        return 1;
    }

    if (a & b | c) {
        return 1;
    }

    if (a & b ^ c) {
        return 1;
    }

    if (a | b ^ c) {
        return 1;
    }

    if (a | b & c) {
        return 1;
    }

    if (a ^ b & c) {
        return 1;
    }

    return 0;
}