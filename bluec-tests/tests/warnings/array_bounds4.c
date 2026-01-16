long x[4] = {1, 2, 3, 4};
long *ptr = &x[3 * 2];

int main(void) {
    return (int)*ptr;
}