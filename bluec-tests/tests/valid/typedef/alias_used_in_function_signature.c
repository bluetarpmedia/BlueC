typedef int MyInt;
typedef MyInt Another;
typedef Another YetAgain;

YetAgain calc(MyInt age, Another fav);

int calc(int age, int height) {
    return age + height;
}

int main(void) {
    return calc(15, 6);
}