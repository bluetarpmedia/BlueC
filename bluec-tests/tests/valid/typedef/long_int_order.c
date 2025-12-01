int main(void) {
    typedef long int Long1;
    typedef long     Long2;
    typedef int long Long3;
    int typedef long Long4;
    long typedef int Long5;
    int long typedef Long6;
    long int typedef Long7;

    Long1 a = 1;
    Long2 b = 1;
    Long3 c = 1;
    Long4 d = 1;
    Long5 e = 1;
    Long6 f = 1;
    Long7 g = 1;
    
    return a + b + c + d + e + f + g;
}