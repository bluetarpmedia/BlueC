int main(void) {
    /* It's illegal to XOR floats */
    return 1e10f ^ -1e10f;
}