int x;

int main() {
    switch (x) {
        case 2:
            printstr("Here's a 2!");
            break;
        default:
            printint(x);
        case 4:
            break;
    }

    return 0;
}