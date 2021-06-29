use crate::parser::{ASTnode, ASTop};

pub fn print_tree(node: &ASTnode, indentation_count: u8) {
    print!("{}{}", " ".repeat(indentation_count.into()), node.op.name());

    if node.op == ASTop::INTLIT {
        println!(": {}", node.int_value);
    } else {
        println!("");
    }

    if let Some(left) = &node.left {
        print_tree(left.as_ref(), indentation_count + 2);
    }

    if let Some(right) = &node.right {
        print_tree(right.as_ref(), indentation_count + 2);
    }
}
