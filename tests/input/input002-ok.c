int x;
int y;

int main() {
    x = 5;
    y = 7;
    printint(1 + x * 2);
    printint(y * 3);
    x = 1 - 7 * y - x;
    printint(x);

    return 1;
}
