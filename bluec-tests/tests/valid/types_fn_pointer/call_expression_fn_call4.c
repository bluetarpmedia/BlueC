typedef int (*FnPtr)(void);
typedef FnPtr (*FnFnPtr)(void);

int get_value(void) {
    return 111;
}

FnPtr get_another_fn(void) {
    return get_value;
}

FnFnPtr get_fn(void) {
    return get_another_fn;
}

int main(void) {
    int (*fn)(void) = 0;

    int v = ( get_fn()() ) ();

    if (v != 111) {
        return 1;
    }

    return 0;
}