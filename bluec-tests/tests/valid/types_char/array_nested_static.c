int strcmp(char *s1, char *s2);

char nested1[2][2][2] = {{"ab"}, {"1", "x"}};
char nested2[2][2][3] = {{"abc"}, {"1", "xy"}};
char nested3[2][2][4] = {{"abcd"}, {"12", "xyz"}};

int test1(void) {
    char concat[9] = {0};
    int idx = 0;

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < 2; ++j) {
            for (int k = 0; k < 2; ++k) {
                char c = nested1[i][j][k];

                if (c) {
                    concat[idx++] = c;
                } else {
                    concat[idx++] = '.';
                }
            }
        }
    }

    return strcmp(concat, "ab..1.x.");
}

int test2(void) {
    char concat[13] = {0};
    int idx = 0;

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < 2; ++j) {
            for (int k = 0; k < 3; ++k) {
                char c = nested2[i][j][k];

                if (c) {
                    concat[idx++] = c;
                } else {
                    concat[idx++] = '.';
                }
            }
        }
    }

    return strcmp(concat, "abc...1..xy.");
}

int test3(void) {
    char concat[17] = {0};
    int idx = 0;

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < 2; ++j) {
            for (int k = 0; k < 4; ++k) {
                char c = nested3[i][j][k];

                if (c) {
                    concat[idx++] = c;
                } else {
                    concat[idx++] = '.';
                }
            }
        }
    }

    return strcmp(concat, "abcd....12..xyz.");
}

int main(void) {
    if (test1()) {
        return 1;
    }

    if (test2()) {
        return 2;
    }

    if (test3()) {
        return 3;
    }

    return 0;
}