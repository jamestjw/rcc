int y[1];
int *x;


int main() {
    y[0] = 42;
    printint(y[0]);
    
    x = &y[0];

    printint(*x);
    *x = 512;
    
    printint(y[0]);
    printint(*x);

    return 0;
}
