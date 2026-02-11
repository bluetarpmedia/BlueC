int main(void) {
    char char_array[4] = {-1, +1, !1, ~1};

    if (char_array[0] != -1) {
        return 1;
    }

    if (char_array[1] != 1) {
        return 2;
    }

    if (char_array[2] != 0) {
        return 3;
    }

    if (char_array[3] != -2) {
        return 4;
    }

    return 0;
}