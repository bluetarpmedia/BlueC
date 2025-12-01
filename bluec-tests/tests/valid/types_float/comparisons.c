float fifty_fiveE5 = 55e5f;
float fifty_fourE4 = 54e4f;
float tiny = .00004f;
float four = 4.f;
float point_one = 0.1f;

/* Test comparisons on floats - evaluate true and false case for each comparison operator */
int main(void) {

    /* false comparisons */
    if (fifty_fiveE5 < fifty_fourE4) {
        return 1;
    }

    if (four > 4.0f) {
        return 2;
    }

    if (tiny <= 0.0f) {
        return 3;
    }

    if (fifty_fourE4 >= fifty_fiveE5) {
        return 4;
    }

    if (tiny == 0.0f) {
        return 5;
    }

    if (point_one != point_one) {
        return 6;
    }

    /* true comparisons */

    if (!(tiny > 00.000005f))  {
        return 7;
    }

    if (!(-.00004f < four)) {
        return 8;
    }

    if (!(tiny <= tiny)) {
        return 9;
    }

    if (!(fifty_fiveE5 >= fifty_fiveE5)) {
        return 10;
    }

    if (!(0.1f == point_one)) {
        return 11;
    }

    if (!(tiny != .00003f)) {
        return 12;
    }

    /* try comparing two constants */
    if (0.00003f < 0.000000000003f) {
        return 13;
    }

    return 0;

}