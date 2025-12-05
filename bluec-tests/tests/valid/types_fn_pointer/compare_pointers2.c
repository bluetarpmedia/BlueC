int secret(void) {
    return 32;
}

int (*get_null_fn_pointer(void))(void) {
    return 0;
}

int (*get_fn_pointer(void))(void) {
    return secret;
}

int main(void)
{
    int (*fn)(void) = 0;

    if (fn != 0) {
        return 1;
    }

    fn = get_fn_pointer();
    
    if (fn != &secret) {
        return 2;
    }

    if (fn != secret) {
        return 3;
    }

    fn = get_null_fn_pointer();
    if (fn != 0) {
        return 4;
    }

    return 0;
}