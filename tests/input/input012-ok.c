int x;

void increment_x() {
    x = x + 1;
}

int main() {
    x = 10;
    printint(x);
    increment_x();
    printint(x);
    return 0;
}
