int get_int(void) {
    return 1;
}

float get_float(void) {
    return 3.0f;
}

int main(void) {
    return ((1 > 2) ? get_int : get_float)();
}