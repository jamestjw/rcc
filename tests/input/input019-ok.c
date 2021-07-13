char *x;
char y;

int main() {
    x = &y;
    *x = 'C';
    printchar(*x);
    printchar(y);

    y = y + 1;
    printchar(*x);
    printchar(y);
    return 0;
}