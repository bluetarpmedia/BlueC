int main(void) {
    if (_Alignof(char[50]) != 1) {
        return 1;
    }

    if (_Alignof(unsigned short[16]) != 2) {
        return 2;
    }

    if (_Alignof(int[100]) != 4) {
        return 3;
    }

    if (_Alignof(unsigned long[44]) != 8) {
        return 4;
    }

    if (_Alignof(float[44]) != 4) {
        return 5;
    }

    if (_Alignof(double[44]) != 8) {
        return 6;
    }

    if (_Alignof(int[44][33][22]) != 4) {
        return 7;
    }

    if (_Alignof(unsigned long long[44][33][22]) != 8) {
        return 8;
    }

    return 0;
}