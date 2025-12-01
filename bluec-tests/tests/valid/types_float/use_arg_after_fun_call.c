/* In this function, the parameter x and
 * the return value are both passed in XMM0.
 * We make a recursive call to fun, then use x,
 * to make sure that x's value is preserved
 * across the function call.
 */
float fun(float x) {
    if (x > 2)
        return x;
    else {
        float ret = fun(x + 2); // ret = 3.0f
        return ret + x; // return 4.0f
    }
}

int main(void) {
    return fun(1.0f);
}