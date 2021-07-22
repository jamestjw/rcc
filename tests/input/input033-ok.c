int x;

int main() {
    x = 10;
    while (1) {
        printint(x);
        x = x - 1;
        if (x == 5) {
            printstr("Found a 5!");
            break;
        }
    }
    return 0;
}