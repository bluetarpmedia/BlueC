// Test reading and writing a local static float

// Return old value, then increment by one
float return_static_variable(void) {
    static float f = 0.5f;
    float ret = f;
    f = f + 1.0f;
    return ret;
}

int main(void) {
    float f1 = return_static_variable();
    float f2 = return_static_variable();
    float f3 = return_static_variable();
    if (f1 != 0.5f) {
        return 1;
    }
    if (f2 != 1.5f) {
        return 2;
    }
    if (f3 != 2.5f) {
        return 3;
    }
    return 0;
}
