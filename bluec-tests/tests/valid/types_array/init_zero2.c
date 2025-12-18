int main(void) {
    static int a[3] = {0};
    static int b[3][3] = {0};
    static int c[3][3][3] = {0};

    for (int i = 0; i < 3; ++i) {
        if (a[i] != 0) {
            return 1;
        }

        for (int j = 0; j < 3; ++j) {
            if (b[i][j] != 0) {
                return 2;
            }

            for (int k = 0; k < 3; ++k) {
                if (c[i][j][k] != 0) {
                    return 3;
                }
            }
        }
    }
    
    return 0;
}