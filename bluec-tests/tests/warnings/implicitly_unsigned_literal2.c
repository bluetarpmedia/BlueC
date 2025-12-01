int main(void) {
    // Literal without suffix is implicitly interpreted as signed, but
    // it's too large for a signed 32- or 64-bit integer, so it's
    // promoted to 64-bit 'unsigned long'. Then the 'ul' literal is
    // implicitly casted to 'long' because of the type of 'a',
    // so we expect 2 warnings.
    //
    long a = 9323372036854775707;
    return a == -9123372036854775909;
}
