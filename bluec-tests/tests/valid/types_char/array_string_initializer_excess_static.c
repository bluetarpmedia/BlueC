int main(void) {
    static char too_long[3] = "abcd";
    return too_long[2] == 'c';
}