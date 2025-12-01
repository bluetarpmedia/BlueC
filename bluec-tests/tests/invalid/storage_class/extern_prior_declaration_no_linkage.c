// See gcc bug 90472
// The following program is ill-formed and a diagnostic is permitted but not required.
// See C17 6.2.2

static int a = 10;

int main(void) {
    // This local variable has no linkage.
    // If this declaration was omitted then the subsequent 'extern a' would refer to the
    // static declaration at file-scope, and take its internal linkage.
    // But since it exists, the following applies:
    int a = 1;
    {
        // "If no prior declaration is visible, or if the prior declaration specifies no linkage,
        //  then the identifier has external linkage."
        extern int a;
        return a;
    }
    return 0;
}