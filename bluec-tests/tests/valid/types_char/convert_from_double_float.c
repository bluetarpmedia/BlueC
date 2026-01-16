int main(void) {
    double d = 65.0;
    float f = 65.0f;

    char c1 = d;
    char c2 = f;

    unsigned char uc1 = d;
    unsigned char uc2 = f;

    char valid_char = c1 == c2 && c1 == 'A';
    char valid_uchar = uc1 == uc2 && uc1 == 'A';

    if (!valid_char) {
        return 1;
    }

    if (!valid_uchar) {
        return 2;
    }

    return 0;
}