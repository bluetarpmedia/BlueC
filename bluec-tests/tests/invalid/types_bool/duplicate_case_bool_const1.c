int main(void) {
    _Bool b = 1;
    switch (b) {
        case 0:
            return 1;
        case 100 - 100:
            return 2;
        default:
            return 3;
    }
}