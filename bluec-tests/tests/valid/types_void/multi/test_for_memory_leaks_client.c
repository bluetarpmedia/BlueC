extern long sum;
void lots_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o);

int main(void) {
    // call lots_of_args many times; if we aren't deallocating arguments and padding correctly, it will segfault
    for (int i = 0; i < 10000000; i = i + 1) {
        lots_of_args(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, i);
    }
    if (sum != 49999995000000) {
        return 15;
    }
    return 0;
}