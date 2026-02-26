#define EXPECT_TRUE(value, test_no) \
    do { \
        _Bool converted_bool = value; \
        if (converted_bool != 1) { \
            return test_no; \
        } \
    } while(0)

int main(void) {
    _Bool b = 1;
    char c = -1;
    unsigned char uc = 'b';
    short s = -12;
    unsigned short us = 2;
    int i = -100;
    unsigned int ui = 2;
    int l = -12345;
    unsigned int ul = 3;

    EXPECT_TRUE(b, 1);
    EXPECT_TRUE(c, 2);
    EXPECT_TRUE(uc, 3);
    EXPECT_TRUE(s, 4);
    EXPECT_TRUE(us, 5);
    EXPECT_TRUE(i, 6);
    EXPECT_TRUE(ui, 7);
    EXPECT_TRUE(l, 8);
    EXPECT_TRUE(ul, 9);

    float f1 = 3.0f;
    float f2 = -4.0f;
    double d1 = 5.0;
    double d2 = -6.0;

    EXPECT_TRUE(f1, 10);
    EXPECT_TRUE(f2, 11);
    EXPECT_TRUE(d1, 12);
    EXPECT_TRUE(d2, 13);

    char text[5] = "test";
    int values[3] = {1, 2, 3};
    int zero_len_array[0];

    EXPECT_TRUE(text, 14);
    EXPECT_TRUE(values, 15);
    EXPECT_TRUE(zero_len_array, 16);

    void *pv = &i;
    int *pi = &i;

    EXPECT_TRUE(pv, 17);
    EXPECT_TRUE(pi, 18);

    return 0;
}
