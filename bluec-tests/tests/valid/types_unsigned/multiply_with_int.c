
int main(void) {
    int a = 1;

    a = (4294967195u + 1u) * a;
    if (a != -100) {
        return 1;
    }

    a = 1;
    a = a * (4294967195u + 1u);
    if (a != -100) {
        return 2;
    }

    a = 1;
    a *= 4294967195u + 1u;
    if (a != -100) {
        return 3;
    }

    return 0;
}
