int x;

int main() {
    for (x = 0; x < 10; x = x + 1) {
        if (x == 5) {
            printstr("Skipping a 5!");
            continue;
        }
        
        printint(x);
    }

    return 0;
}