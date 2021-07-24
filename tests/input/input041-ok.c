int x;

int main() {
    for (x = 2;;) {
        if (x == 5) {
            printstr("Skipping a 5!");
            x = x + 1;
            continue;
        }
        
        printint(x);

        if (x == 10) {
            break;
        }
        x = x + 1;
    }

    return 0;
}