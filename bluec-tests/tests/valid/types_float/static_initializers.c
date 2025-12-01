/* Test initializing static doubles with integer constants and vice versa */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wimplicit-const-int-float-conversion"
#pragma GCC diagnostic ignored "-Wliteral-conversion"
#endif
#endif

// float variables

// can convert from int/uint without rounding
float f1 = 2147483647;
float f2 = 4294967295u;

/* midway point between 4611686018427388928.0 and 4611686018427389952.0
 * We round ties to even, so round this up to 4611686018427389952.0
 */
float f3 = 4611686018427389440l;

/* We'll round this down to 4611686018427389952.0 */
float f4 = 4611686018427389955l;

/* Using round-to-nearest, this rounds to 9223372036854775808 */
float f5 = 9223372036854775810ul;
float f6 = 4611686018427389955ul; // this as the same value as d4 and should round to the same float

/* This is exactly halfway between 9223372036854775808.0 and
 * 9223372036854777856.0 We round ties to even, so this
 * rounds down to 9223372036854775808.0
 */
float f7 = 9223372036854776832ul;

float uninitialized; // should be initialized to 0.0

// integer variables

static int i = 4.9f; // truncated to 4

int unsigned u = 42.67E5f; // truncated to 4267000u

// this token is first converted to a float w/ value 4611686018427389952.0,
// then truncated down to long 4611686018427389952
long l = 4611686018427389440.;

unsigned long ul = 18446744073709549568.;

int main(void) {
    if (f1 != 2147483647.f) {
        return 1;
    }

    if (f2 != 4294967295.f) {
        return 2;
    }
    if (f3 != 4611686018427389952.f) {
        return 3;
    }

    if (f4 != f3) {
        return 4;
    }

    if (f5 != 9223372036854775808.f) {
        return 5;
    }

    if (f6 != f3) {
        return 6;
    }

    if (f7 != f5) {
        return 7;
    }

    if (uninitialized) {
        return 8;
    }

    if (i != 4) {
        return 9;
    }

    if (u != 4267000u) {
        return 10;
    }

    if (l != 4611686018427389952l) {
        return 11;
    }

    if (ul != 18446744073709549568ul) {
        return 12;
    }

    return 0;
}