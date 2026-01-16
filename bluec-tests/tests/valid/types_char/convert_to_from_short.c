int main(void) {
    char a = 'A';
    unsigned char b = 'A';

    short s1 = a;
    short s2 = b;

    unsigned short us1 = a;
    unsigned short us2 = b;

    char valid_short = s1 == s2 && s1 == 65;
    char valid_ushort = us1 == us2 && us1 == 65;

    if (!valid_short) {
        return 1;
    }

    if (!valid_ushort) {
        return 2;
    }

    char c1 = s1;
    char c2 = us1;

    unsigned char uc1 = s1;
    unsigned char uc2 = us1;

    char valid_char = c1 == c2 && c1 == 'A';
    char valid_uchar = uc1 == uc2 && uc1 == 'A';

    if (!valid_char) {
        return 3;
    }

    if (!valid_uchar) {
        return 4;
    }

    return 0;
}