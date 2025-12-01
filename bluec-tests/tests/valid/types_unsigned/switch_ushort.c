#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wswitch"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

int switch_on_ushort(unsigned short us) {
    switch (us) {
        case 5:
            return 0;
        // this will be converted to an unsigned short, preserving its value
        case 60000l:
            return 1;
        // 2^35 + 10, will be converted to 10
        case 34359738378ul:
            return 2;
        default:
            return 3;
    }
}

int main(void) {
    if (switch_on_ushort(5) != 0)
        return 1;
    if (switch_on_ushort(60000) != 1)
        return 2;
    if (switch_on_ushort(10) != 2)
        return 3;
    return 0;
}