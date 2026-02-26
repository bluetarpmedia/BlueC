#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wswitch"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// Make sure we promote the controlling condition in a switch statement from
// _Bool type to int

int main(void) {
    _Bool b = 1;

    switch (b) {
        case 0:
            return 1;
        case 1:
            return 0;
        case 256:
            return 2;
        default:
            return 3;
    }
}