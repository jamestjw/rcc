void test(int x) {
    if (x) {
        printstr("is not zero");
    } else {
        printstr("is zero");
    }
}

int main() {
    test(10);
    test(0);
    test(-10);
    return 0;
}