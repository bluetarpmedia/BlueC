int main(void) {
    char char_array[4] = {'b' - 1, 'a' + 1, 'a' + 2, 'a' + 3};

    if (char_array[0] != 'a') {
        return 1;
    }

    if (char_array[1] != 'b') {
        return 2;
    }

    if (char_array[2] != 'c') {
        return 3;
    }

    if (char_array[3] != 'd') {
        return 4;
    }

    return 0;
}