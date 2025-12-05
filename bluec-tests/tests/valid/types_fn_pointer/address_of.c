float add(float a, float b) {
    return a + b;
}

int main(void) {
    float (*fn1)(float, float) = add;
    float (*fn2)(float, float) = &add;

    float a = add(1, 2);
    float b = fn1(1, 2);
    float c = (*fn1)(1, 2);
    float d = fn2(1, 2);
    float e = (*fn2)(1, 2);

    if (a != 3.0f) {
        return 1;
    }

    if (b != 3.0f) {
        return 2;
    }

    if (c != 3.0f) {
        return 3;
    }

    if (d != 3.0f) {
        return 4;
    }

    if (e != 3.0f) {
        return 5;
    }

    return 0;
}
