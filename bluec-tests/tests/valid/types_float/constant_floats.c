int main(void) {
    /* Define constant floats in a few different formats,
     * and make sure we can lex all of them.
     * Note that these can all be represented exactly,
     * without rounding.
     */

    /* Several ways to define 1 */
    float a = 1.0f;
    float b = 1.f;
    float c = 1E0f;
    float d = .01e+2f;

    /* Make sure they all have the correct value */
    if (! (a == b && a == c && a == d) )
        return 1;

    if (a + b + c + d != 4.0f)
        return 2;

    /* Several ways to define .125 */
    float e = .125f;
    float f = 12.5e-2f;
    float g = 125.E-3f;
    float h = 1250000000e-10f;

    /* Make sure they all have the correct value */
    if (! (e == f && e == g && e == h) )
        return 3;

    if (e + f + g + h != 0.5f)
        return 4;

    return 0;

}
