int main(void) {
    // Literal without suffix is implicitly interpreted as signed, but
    // it's too large for a signed 32- or 64-bit integer, so it's
    // promoted to 64-bit 'unsigned long'.
    //
    unsigned long b = 9323372036854775707;
    return b == 9323372036854775707ull;
}
