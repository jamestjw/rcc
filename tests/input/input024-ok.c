struct Person {
    int id;
    char *name;
} person1;

int main() {
    person1.id = 5;
    printint(person1.id);
    person1.id = person1.id * 2;
    printint(person1.id);
    
    
    person1.name = "John";
    printstr(person1.name);

    person1.name = "Jake";
    printstr(person1.name);
    return 0;
}