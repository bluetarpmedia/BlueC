int main(void) {
    char char_array[4] = { -'a', +'a', ~'a', !'a' };

    if (char_array[0] != -97) {
        return 1;
    }

    if (char_array[1] != 97) {
        return 2;
    }

    if (char_array[2] != -98) {
        return 3;
    }

    if (char_array[3] != 0) {
        return 4;
    }

    return 0;
}