int main(void) {
    char array[3][3] = {"a", "bcde"};
    return array[1][2] == 'd' && array[2][0] == 0;
}