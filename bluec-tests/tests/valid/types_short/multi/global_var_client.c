// the following are defined in global_var.c
extern short int s;
short return_s(void);
int return_s_as_int(void);



int main(void) {

    /* Make sure s has the right value before we update it */
    if (return_s() != 30000)
        return 1;

    if (return_s_as_int() != 30000)
        return 2;

    /* Update s */
    s = s + 10;

    /* Read back the value we just assigned to s */
    if (return_s() != 30010)
        return 3;

    if (return_s_as_int() != 30010)
        return 4;

    return 0;
}