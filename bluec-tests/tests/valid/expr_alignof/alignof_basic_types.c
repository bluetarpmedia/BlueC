int main(void) {
    if (_Alignof(char) != 1) {
        return 1;
    }

    if (_Alignof(signed char) != 1) {
        return 2;
    }

    if (_Alignof(unsigned char) != 1) {
        return 3;
    }

    if (_Alignof(short) != 2) {
        return 4;
    }
    if (_Alignof(unsigned short) != 2) {
        return 5;
    }

    if (_Alignof(int) != 4) {
        return 6;
    }
    if (_Alignof(unsigned int) != 4) {
        return 7;
    }

    if (_Alignof(long) != 8) {
        return 8;
    }
    if (_Alignof(unsigned long) != 8) {
        return 9;
    }

    if (_Alignof(float) != 4) {
        return 10;
    }

    if (_Alignof(double) != 8) {
        return 11;
    }

    return 0;
}