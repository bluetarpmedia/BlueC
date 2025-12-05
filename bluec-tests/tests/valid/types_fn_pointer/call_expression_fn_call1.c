typedef int (*FnPtr)(void);

int get_value(void) {
    return 111;
}

FnPtr get_fn(void) {
    return get_value;
}

int main(void) {
    int (*fn)(void) = 0;
    int v = 0;
    
    v = get_fn()();

    if (v != 111) {
        return 1;
    }

    return 0;
}