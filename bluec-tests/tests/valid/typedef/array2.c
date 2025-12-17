int main(void) {
    typedef int My2DArray[3][3];
    
    My2DArray arr1 = { {1, 2, 3}, {4, 5, 6}, {7, 8, 9},};
    My2DArray arr2 = { {10, 20, 30}, {40, 50, 60}, {70, 80, 90},};
    My2DArray arr3 = {};

    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            arr3[i][j] = arr1[i][j] + arr2[i][j];
        }
    }

    return arr3[2][2];
}