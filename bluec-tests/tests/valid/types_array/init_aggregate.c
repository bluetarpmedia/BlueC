// Should be initialized as {3, 4, 5}.
int a[3] = { 3, 4, 5 };
int b[3] = { 3, {4}, {{5}} };
int c[3] = { 3, {{{4}}}, {5} };
int d[3] = { {{{3}}}, 4, 5 };

// Should be initialized as {3, 0, 0}.
int e[3] = { {{{3}}} };
int f[3] = { {3, 4, 5} };
int g[3] = { 3, {}, {} };
int h[3] = { 3, {{}}, {{{}}} };

int x = {11, 22, 33}; // Should be initialized to 11.

int check_345(int *arr) {
    return arr[0] == 3 && arr[1] == 4 && arr[2] == 5;
}

int check_300(int *arr) {
    return arr[0] == 3 && arr[1] == 0 && arr[2] == 0;
}

int main(void) {
    // As above, but locals on stack.
    int loc_a[3] = { 3, 4, 5 };
    int loc_b[3] = { 3, {4}, {{5}} };
    int loc_c[3] = { 3, {{{4}}}, {5} };
    int loc_d[3] = { {{{3}}}, 4, 5 };

    int loc_e[3] = { {{{3}}} };
    int loc_f[3] = { {3, 4, 5} };
    int loc_g[3] = { 3, {}, {} };
    int loc_h[3] = { 3, {{}}, {{{}}} };

    int loc_x = {11, 22, 33};
    
    int valid_global_345 = check_345(a) && check_345(b) && check_345(c) && check_345(d);
    int valid_global_300 = check_300(e) && check_300(f) && check_300(g) && check_300(h);
    int valid_global_x = (x == 11);

    int valid_loc_345 = check_345(loc_a) && check_345(loc_b) && check_345(loc_c) && check_345(loc_d);
    int valid_loc_300 = check_300(loc_e) && check_300(loc_f) && check_300(loc_g) && check_300(loc_h);
    int valid_loc_x = (loc_x == 11);

    return valid_global_345
        && valid_global_300
        && valid_global_x
        && valid_loc_345
        && valid_loc_300
        && valid_loc_x;
}
