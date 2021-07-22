void compare(int x) {
    if (x > 5) {
        if (x > 10) {
            printstr("More than 10");
        } else {
            printstr("More than 5");
        }
    } else {
        printstr("Not more than 5");
    }
    
    if (x >= 5) {
        if (x >= 10) {
            printstr("More than or equal to 10");
        } else {
            printstr("More than or equal to 5");
        } 
    } else {
        printstr("Not more than or equal to 5");
    }
    
    
    if (x < 5) {
        printstr("Less than 5");
    }
    if (x <= 5) {
        printstr("Less than or equal to 5");
    }
    if (x == 5) {
        printstr("Equals to 5");
    }
    if (x != 5) {
        printstr("Not equal to 5");
    }
}

int main() {
    printstr("5 is");
    compare(5);
    printstr("---");
    printstr("10 is");
    compare(10);
    printstr("---");
    printstr("0 is");
    compare(0);
    printstr("---");
    printstr("-5 is");
    compare(-5);
    return 0;
}