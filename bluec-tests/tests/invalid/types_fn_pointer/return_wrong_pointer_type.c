int i = 0;

int (*return_fn_pointer(void))(void) {
    return &i;
}

int main(void) {
    int *i = return_fn_pointer();
    return 0;
}