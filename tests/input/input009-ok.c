int x;
int y;

int add(int i, int j) {
    return i + j;
}

int main() {
    y = 2;
    x = add(10, 15);
    print(x);

    x = add(x, y);
    print(x);

    return 0;
}
