/* Basic test of two forms of sizeof: referring to type names and expressions */

int main(void) {
    if (sizeof (int) != 4) {
        return 1;
    }

    if (sizeof 1.0f != 4) {
        return 2;
    }

    if (sizeof 3.0 != 8) {
        return 3;
    }

    return 0;
}