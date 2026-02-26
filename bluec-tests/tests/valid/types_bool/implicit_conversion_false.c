#define EXPECT_FALSE(value, test_no) \
    do { \
        _Bool converted_bool = value; \
        if (converted_bool != 0) { \
            return test_no; \
        } \
    } while(0)

int main(void) {
    _Bool b = 0;
    char c = 0;
    unsigned char uc = 0;
    short s = 0;
    unsigned short us = 0;
    int i = 0;
    unsigned int ui = 0;
    int l = 0;
    unsigned int ul = 0;

    EXPECT_FALSE(b, 1);
    EXPECT_FALSE(c, 2);
    EXPECT_FALSE(uc, 3);
    EXPECT_FALSE(s, 4);
    EXPECT_FALSE(us, 5);
    EXPECT_FALSE(i, 6);
    EXPECT_FALSE(ui, 7);
    EXPECT_FALSE(l, 8);
    EXPECT_FALSE(ul, 9);

    float f1 = 0.0f;
    float f2 = -0.0f;
    double d1 = 0.0;
    double d2 = -0.0;

    EXPECT_FALSE(f1, 10);
    EXPECT_FALSE(f2, 11);
    EXPECT_FALSE(d1, 12);
    EXPECT_FALSE(d2, 13);

    void *pv = 0;
    int *pi = 0;

    EXPECT_FALSE(pv, 14);
    EXPECT_FALSE(pi, 15);

    return 0;
}
