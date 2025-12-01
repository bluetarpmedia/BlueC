int main(void) {

    /* multiplication promotion to int */
    short m = 2000;
    short n = 2000;
    if (m * n != 4000000) return 1;

    /* integer division positive operands (7 / 2 == 3) */
    short d1 = 7;
    short d2 = 2;
    if (d1 / d2 != 3) return 2;

    /* integer division negative numerator (-7 / 2 == -3) */
    short d3 = -7;
    short d4 = 2;
    if (d3 / d4 != -3) return 3;

    /* integer division negative denominator (7 / -2 == -3) */
    short d5 = 7;
    short d6 = -2;
    if (d5 / d6 != -3) return 4;

    /* integer division both negative (-7 / -2 == 3) */
    short d7 = -7;
    short d8 = -2;
    if (d7 / d8 != 3) return 5;

    /* division combined with promotion and multiplication ( (30000 * 2) / 3 == 20000 ) */
    short dv1 = 30000;
    short dv2 = 2;
    short dv3 = 3;
    if ((dv1 * dv2) / dv3 != 20000) return 6;

    return 0;
}
