struct Person {
    int id;
    char *name;
};

struct Person person1;
struct Person *person2;

void set_person_id(struct Person *p, int id) {
    p->id = id;
}

int main() {   
    person2 = &person1;

    person2->id = 2;
    printint(person2->id);

    set_person_id(person2, 5);
    printint(person2->id);

    return 0;
}