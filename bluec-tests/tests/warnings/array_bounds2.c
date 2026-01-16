long x[4] = {1, 2, 3, 4};
long *ptr = &x[4 - 5];

int main(void) {
    return (int)*ptr;
}