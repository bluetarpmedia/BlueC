// can't apply sizeof to a function
int main(void) {
    return sizeof (int (float, double));
}