int x;

int main() {
    x = 5;
    if (x) {
        printstr("This should be printed");
    }
    
    x = 1;
    if (x) {
        printstr("This should also be printed");
    }

    x = -5;
    if (x) {
        printstr("This as well should be printed");
    }

    x = 0;
    if (x) {
        printstr("This should not be printed");
    }
    
    return 0;
}