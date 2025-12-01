/* Make sure we can read and write long integers in other translation units */

// the following are defined in longlong_global_var.c
extern long long int l;
long long return_l(void);
int return_l_as_int(void);



int main(void) {

    /* Make sure l has the right value before we update it */
    if (return_l() != 8589934592LL)
        return 1;

    if (return_l_as_int() != 0)
        return 2;

    /* Update l */
    l = l - 10LL;

    /* Read back the value we just assigned to l */
    if (return_l() != 8589934582LL)
        return 3;

    if (return_l_as_int() != -10)
        return 4;

    return 0;
}