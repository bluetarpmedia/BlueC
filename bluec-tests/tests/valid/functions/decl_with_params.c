int calc(int (a), int (b));
int (calc)(int, int);
int ((calc))(int, int);
int ((calc))(int ((a)), int ((b)));
int ((calc)(int, int));

int main(void) {
    if (calc(1, 2) != 3) {
        return 1;
    }

    return 0;
}

int ((calc))(int (a), int (b)) {
    return a + b;
}
