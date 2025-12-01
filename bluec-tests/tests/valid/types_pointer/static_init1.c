int value = 55;
int *a = 0;
int *b = (int *)12;
int *c = &value;

int main(void)
{
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
