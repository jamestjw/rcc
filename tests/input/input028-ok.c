struct Person {
    int id;
    char *name;
};
struct Person person1;
struct Person *person2;

int main() {
    person2 = &person1;
    
    person2->id = 5;
    printint(person2->id);
    printint(person1.id);
    person1.id = 10;
    printint(person2->id);
    printint(person1.id);

    person2->id = person2->id * 2;
    printint(person2->id);
    printint(person1.id);
    person1.id = person1.id * 4;
    printint(person2->id);
    printint(person1.id);
    
    person2->name = "John";
    printstr(person2->name);
    printstr(person1.name);

    person1.name = "Jake";
    printstr(person2->name);
    printstr(person1.name);
    return 0;
}