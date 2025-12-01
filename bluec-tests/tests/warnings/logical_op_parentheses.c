int main(void) {
    int a = 1, b = 2, c = 3;

    if (a || (b && c) || b) { // No warning
        return 0;
    }

    if ((a && b) || (c && b)) { // No warning
        return 0;
    }

    if (a && b || c) {
        return 0;
    }

    if (a || b && c) {
        return 0;
    }

    if (a && b || c && b) {
        return 0;
    }

    return 0;
}