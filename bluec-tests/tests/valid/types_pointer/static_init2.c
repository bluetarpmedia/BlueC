int value = 55;

int main(void)
{
    static int *a = 0;
    static int *b = (int *)12;
    static int *c = &value;
    static int *d = 1 + &value - 1;

    if (a != 0) {
        return 1;
    }

    if (b != 12) {
        return 2;
    }

    if (*c != 55) {
        return 3;
    }

    if (*d != 55) {
        return 4;
    }

    return 0;
}
