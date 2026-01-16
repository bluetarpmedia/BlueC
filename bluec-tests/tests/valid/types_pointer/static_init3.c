long a[4] = {11, 22, 33, 44};

long *p1 = &a[0];
long *p2 = &a[0] + 3;
long *p3 = 3 + &a[3] - 3;

float b[6] = {0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f};

float *q1 = &b[6] - 1;
float *q2 = &b[6 - 1];
float *q3 = &b[-1] + 1;

long diff = &b[6] - &b[0];

int main(void)
{
    if (*p1 != 11) {
        return 1;
    }

    if (*p2 != 44) {
        return 2;
    }

    if (p2 != p3) {
        return 3;
    }

    if (*p3 != 44) {
        return 4;
    }

    if (q1 != q2) {
        return 5;
    }

    if (*q1 != 0.6f) {
        return 6;
    }

    if (*q3 != 0.1f) {
        return 7;
    }

    if (diff != 6) {
        return 8;
    }

    return 0;
}
