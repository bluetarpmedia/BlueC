int main(void) {
    _Bool t = 1;
    _Bool f = 0;
    _Bool res;

    if ((t && f) != 0) return 1;
    if ((t || f) != 1) return 2;

    res = t + t;
    if (res != 1) return 3;
    
    res = f - t;
    if (res != 1) return 4;

    res = t * f;
    if (res != 0) return 5;

    if (!(t > f))  return 6;
    if (!(f < t))  return 7;
    if (t == f)    return 8;
    if (t != t)    return 9;

    res = t ^ t;
    if (res != 0) return 10;
    
    res = t | 2; 
    if (res != 1) return 11;

    res = t / t;
    if (res != 1) return 12;

    res = t << 1;
    if (res != 1) return 13;
    
    res = t >> 1;
    if (res != 0) return 14;

    _Bool b = 1111; 
    if (b == 1111) return 15;

    return 0;
}