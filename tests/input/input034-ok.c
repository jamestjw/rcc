int x;

int main() {
    x = 10;
    while (x > 0) {
        printint(x);
        x = x - 1;
        if (x == 5) {
            printstr("Skipping a 5!");
            continue;
        }
    }
    return 0;
}