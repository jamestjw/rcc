int x;

int main() {
    for (x = 0; x < 10; x = x + 1) {
        switch (x) {
            case 2:
                printstr("Here's a 2!");
                break;
            case 4:
                printstr("Here's a 4!");
                break;
            case 6:
            case 8:
                printstr("Here's a 6 or 8!");
                break;
            default:
                printint(x);
        }
    }

    return 0;
}