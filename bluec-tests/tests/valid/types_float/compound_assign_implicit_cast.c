int main(void) {
    float f = 1000.5f;
    /* When we perform compound assignment, we convert both operands
     * to their common type, operate on them, and convert the result to the
     * type of the left operand */
    f += 1000.0f;
    if (f != 2000.5f) {
        return 1;
    }

    unsigned long ul = 16777216ul;
    /* We'll promote ul to float (16777216.0f),
     * then subtract 1000000.0f,
     * which results in 15777216.0f,
     * then convert it back to an unsigned long
     */
    ul -= 1000000.0f;
    if (ul != 15777216ul) {
        return 2;
    }
    
    /* We'll promote i to a float, add .99999,
     * then truncate it back to an int
     */
    int i = 10;
    i += 0.99999f;
    if (i != 10) {
        return 3;
    }

    return 0;
}