/* Test that we follow the calling convention for a float return type */
float f(void) {
    return 1.23e7f;
}

int main(void) {
    float retval = f();
    return retval == 1.23e7f;
}