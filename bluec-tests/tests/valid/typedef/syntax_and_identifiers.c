int main(void) {
    typedef int MyInt;       // Most common
    int typedef AnotherInt;  // Uncommon but valid
    int typedef;             // Useless but valid (emits warning)
    typedef int;             // Useless but valid (emits warning)
    typedef typedef typedef; // Useless but valid (emits warning)

    typedef typedef int typedef typedef VeryStrangeInt; // Valid but warnings for duplicates

    AnotherInt x = 1;
    MyInt y = 1;
    int z = 1;

    VeryStrangeInt strange = 50;

    {
        int MyInt;
        MyInt = 1;
    }

    {
        int MyInt;
        {
            MyInt = 2;
        }
    }

    {
        int MyInt(void);
        MyInt();  // Call without return type to validate parsing of first token
        z = MyInt();
    }

    return x + y + z;
}

int MyInt(void) {
    return 10;
}