int value = 55;

int main(void)
{
    static int *a = 0;
    static int *b = (int *)12;
    static int *c = &value;

    if (a != 0) {
        return 1;
    }

    if (b != 12) {
        return 2;
    }

    if (*c != 55) {
        return 3;
    }

    return 0;
}
