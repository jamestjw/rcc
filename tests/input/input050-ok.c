 enum Person {
    a,
    b = 2,
    c,
    d,
    e = 10,
    f,
};

void print_person(int x) {
    switch (x) {
        case a:
            printstr("Person A");
            break;
        case b:
            printstr("Person B");
            break;
        case c:
            printstr("Person C");
            break;
        default:
            printstr("Person D, E or F");
    }
}

int main() {
    print_person(a);
    print_person(b);
    print_person(c);
    print_person(d);
    print_person(e);
    print_person(f);

    return 0;
}