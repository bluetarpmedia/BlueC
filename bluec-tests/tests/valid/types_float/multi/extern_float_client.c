/* Test linking against a float defined in another file */
extern float f;

int main(void) {
    return f == 1e10;
}