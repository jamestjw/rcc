struct Person {
    int id;
};

struct Person person1;
struct Person *person2;
struct Person *person3;
int *x;

void set_person_id(struct Person *p, int id) {
    p->id = id;
}

int main() {   
    person1.id = 5;
    person2 = &person1;
    x = (int *) person2;
    
    printint(*x);
    printint(person2->id);
    printint(((struct Person *) x)->id);

    person3 = (struct Person *) x;
    printint(person3->id);
    return 0;
}