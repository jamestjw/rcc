int *x;
int y;

int main() {
    x = &y;
    *x = 4;
    printint(*x);
    printint(y);
    y = 2;
    printint(*x);
    printint(y);
    return 0;
}