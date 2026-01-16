/* Test that we promote short types to integers when we're required to */

int add_shorts(short s1, short s2, short s3) {
    // these are all promoted to int, so this won't overflow
    // even if the result is greater than SHRT_MAX (32767)
    return s1 + s2 + s3;
}

int negate(unsigned short us) {
    // this is promoted to an int before being negated
    // so its value will never wrap around to a large unsigned value
    return -us;
}

int complement(unsigned short us) {
    // promoted to int before bitwise NOT
    // result will be negative/large int, not truncated to short
    return ~us;
}

int add_then_div(short a, short b, short c) {
    // all operands promoted to int, so intermediate (a + b) can exceed 32767
    return (a + b) / c;
}

int mixed_multiply(short s, unsigned short u) {
    // both shorts are converted to int instead of converting signed to unsigned
    return s * u;
}

short decrement(short s) {
    // promoted to int, subtracted, then truncated back to short
    s = s - 1;
    return s;
}

int main(void) {
    short a = 20000;
    short b = 10000;
    // 20000 + 20000 + 10000 = 50000. 
    // This exceeds SHRT_MAX (32767), proving promotion to int occurred.
    if (add_shorts(a, a, b) != 50000) {
        return 1;
    }

    unsigned short one = 1;
    // promotion ensures this is -1 (int), not 65535 (unsigned short)
    if (negate(one) != -1) {
        return 2;
    }

    // ~1 in 32-bit int is -2 (0xFFFFFFFE).
    // In 16-bit unsigned, it would have been 65534 (0xFFFE).
    if (complement(one) != -2) {
        return 3;
    }

    short w = 30000;
    short x = 10000;
    short y = 2;
    // Intermediate 40000 exceeds SHRT_MAX, but promotion to int saves it.
    if (add_then_div(w, x, y) != 20000)
        return 4;

    // Mixed signed/unsigned: both promoted to int.
    short ss = -3;
    unsigned short us = 40000;
    if (mixed_multiply(ss, us) != -120000)
        return 5;

    ss = -32768; // SHRT_MIN
    if (ss != -32768) {
        return 6;
    }

    // -32768 - 1 = -32769 (in int). 
    // When truncated back to 16-bit signed short, it wraps to 32767.
    if (decrement(ss) != 32767) {
        return 7;
    }

    return 0;
}