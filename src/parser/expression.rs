// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use std::error::Error;

impl<'a> Parser<'a> {
    // Pratt parser inspired by https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    pub fn expr_by_precedence(&mut self, min_bp: u8) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let tok_type = match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::EOF => {
                    return Err("Syntax error, unnexpected end of input stream.".into())
                }
                _ => tok.token_type,
            },
            None => panic!("Ran out of tokens to parse in expr_by_precedence."),
        };

        // Handle prefix
        let mut left = match prefix_binding_power(tok_type) {
            Some(((), r_bp)) => {
                let op = token_type_to_prefix_op(tok_type);
                self.consume()?;
                let mut right = self.expr_by_precedence(r_bp)?;

                let data_type = match op {
                    ASTop::DEREF => pointer_to(right.data_type)?,
                    ASTop::ADDR => to_pointer(right.data_type)?,
                    _ => right.data_type,
                };

                if op == ASTop::ADDR {
                    match right.op {
                        ASTop::IDENT | ASTop::OFFSET => {
                            right.rvalue = false;
                        }

                        _ => {
                            return Err(
                                format!("May not take address of {}.", right.op.name()).into()
                            );
                        }
                    }
                }
                ASTnode::new_unary(op, right, data_type)
            }
            None => self.primary_expr()?,
        };

        loop {
            let op_tok_type = match &self.current_token {
                Some(tok) => match tok.token_type {
                    TokenType::EOF => break,
                    _ => tok.token_type,
                },
                // TODO: Check if we should expecting a None here
                None => break,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op_tok_type) {
                if l_bp < min_bp {
                    break;
                }

                match op_tok_type {
                    // Handle function calls
                    TokenType::LPAREN => {
                        if left.op != ASTop::IDENT {
                            return Err(format!(
                                "Function calls on '{}' are not allowed.",
                                left.op.name()
                            )
                            .into());
                        }
                        // Safe to unwrap as ASTnodes with IDENT as op
                        // always have references to a symtable entry
                        let mut fn_node =
                            self.parse_func_call(&left.symtable_entry.as_ref().unwrap().borrow())?;
                        fn_node.left = Some(left);
                        left = fn_node;
                    }
                    // Handle array access
                    TokenType::LBRACKET => {
                        // TODO: This should work on anything yielding a pointer type
                        if left.op != ASTop::IDENT {
                            return Err(format!(
                                "Array access on {} is invalid, access by index is only possible with arrays.",
                                left.op.name()
                            )
                            .into());
                        }
                        self.consume()?; // Consume LBRACKET
                        let index_node = self.expr_by_precedence(0)?;
                        self.match_token(TokenType::RBRACKET)?;
                        let arr_ptr_type = left.data_type;
                        let arr_data_type = pointer_to(arr_ptr_type)?;
                        left.rvalue = true;
                        left = ASTnode::new_boxed(ASTop::OFFSET, left, index_node, arr_data_type);
                    }
                    TokenType::DOT => {
                        // TODO: Consider relaxing this first condition, as we only need to make sure
                        // that there is struct symentry, this will be necessary when we want to allow
                        // chaining of the DOT operator.
                        if left.op != ASTop::IDENT {
                            return Err(format!(
                                "Member access on '{}' is invalid, it is only possible with struct variables.",
                                left.op.name()
                            )
                            .into());
                        }

                        // Given the ASTop, the symtable entry should always be present, hence this is safe
                        let struct_var_sym = Rc::clone(left.symtable_entry.as_ref().unwrap());
                        let struct_var_name = &struct_var_sym.borrow().name;
                        if struct_var_sym.borrow().data_type != DataType::STRUCT {
                            return Err(format!(
                                "Member access on '{}' is invalid, it is only possible with struct variables.",
                                struct_var_name
                            )
                            .into());
                        }
                        let struct_sym =
                            Rc::clone(struct_var_sym.borrow().type_sym.as_ref().unwrap());

                        // Consume the DOT and IDENT containing the member name
                        self.consume()?;
                        let member_tok = self.match_token(TokenType::IDENT)?;

                        match struct_sym.borrow().search_member(&member_tok.lexeme) {
                            Some(member_sym) => {
                                left = ASTnode::new_unary(
                                    ASTop::MEMBER,
                                    left,
                                    member_sym.borrow().data_type,
                                );
                                left.symtable_entry = Some(member_sym);
                            }
                            None => {
                                return Err(format!(
                                    "Invalid access to member '{}' in '{}'.",
                                    &member_tok.lexeme, struct_var_name
                                )
                                .into());
                            }
                        };
                    }
                    _ => {
                        self.consume()?;
                        // Here we are making the assumption that normal postfix operators
                        // do not change the data type of their operands
                        let prev_data_type = left.data_type;
                        left = ASTnode::new_unary(
                            token_type_to_postfix_op(op_tok_type),
                            left,
                            prev_data_type,
                        );
                    }
                }

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op_tok_type) {
                // We only proceed if the binding power is more then what the
                // left token is bound with.
                if l_bp < min_bp {
                    break;
                }

                self.consume()?;

                let mut right = self.expr_by_precedence(r_bp)?;

                let op = token_type_to_binary_op(op_tok_type);

                // TODO: Check that data type of operands are valid
                if op == ASTop::ASSIGN {
                    match left.op {
                        ASTop::IDENT | ASTop::DEREF | ASTop::OFFSET | ASTop::MEMBER => {}
                        _ => {
                            return Err(format!("Unable to assign to {}.", left.op.name()).into());
                        }
                    };
                    right.rvalue = true;
                    left.rvalue = false;
                } else {
                    left.rvalue = true;
                    right.rvalue = true;
                }
                let resulting_data_type = left.data_type;

                // TODO: Data type of resulting node should be the wider type
                left = ASTnode::new_boxed(op, left, right, resulting_data_type);
            } else {
                break;
            }
        }
        left.rvalue = true;

        Ok(left)
    }

    pub fn primary_expr(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::LPAREN => {
                    self.consume()?;
                    let expr = self.expr_by_precedence(0)?;
                    self.match_token(TokenType::RPAREN)?;
                    Ok(expr)
                }
                TokenType::IDENT => {
                    let token = self.match_token(TokenType::IDENT)?;

                    let mut symtable_entry = match &self.current_func_sym {
                        Some(n) => n.borrow().search_member(&token.lexeme),
                        None => None,
                    };

                    if symtable_entry.is_none() {
                        symtable_entry = match self
                            .global_symbol_table
                            .find_symbol(&token.lexeme, SymClass::GLOBAL)
                        {
                            Some(new_sym) => Some(new_sym),
                            None => {
                                return Err(format!(
                                    "Referencing undefined variable {}.",
                                    &token.lexeme
                                )
                                .into())
                            }
                        };
                    }

                    let mut node = ASTnode::new_leaf(
                        ASTop::IDENT,
                        0,
                        symtable_entry.as_ref().unwrap().borrow().data_type,
                    );
                    node.symtable_entry = Some(Rc::clone(symtable_entry.as_ref().unwrap()));

                    Ok(node)
                }
                TokenType::INTLIT => {
                    let int_value = self.consume()?.int_value;
                    Ok(ASTnode::new_leaf(ASTop::INTLIT, int_value, DataType::INT))
                }
                TokenType::STRLIT => {
                    let string_table_id = self.consume()?.string_table_id;
                    let mut node = ASTnode::new_leaf(ASTop::STRLIT, 0, DataType::CHARPTR);
                    node.string_table_id = string_table_id;
                    Ok(node)
                }
                _ => Err(format!(
                    "Syntax error, expected primary expression but found '{}' instead.",
                    tok.token_type
                )
                .into()),
            },
            None => {
                return Err(format!(
                    "Syntax error, expected primary expression but no more tokens were found.",
                )
                .into());
            }
        }
    }

    // TODO: Currently only support one parameter with no type checking
    pub fn parse_func_call(
        &mut self,
        sym: &SymbolTableEntry,
    ) -> Result<Box<ASTnode>, Box<dyn Error>> {
        match sym.sym_type {
            SymType::FUNCTION => {}
            _ => {
                return Err(format!("Call to {} which is not a function.", &sym.name).into());
            }
        };

        self.match_token(TokenType::LPAREN)?;
        let mut arg = None;
        let mut member_node = match &sym.members {
            Some(mem) => Some(Rc::clone(mem)),
            None => None,
        };

        loop {
            match self.current_token.as_ref().unwrap().token_type {
                // Loop until we hit RPAREN
                TokenType::RPAREN => {
                    break;
                }
                _ => {
                    // If we do not hit a RPAREN, and member_node does not exist,
                    // it means that we have been given too many arguments
                    let param_node = match member_node {
                        None => {
                            return Err("Function does not accept this many arguments.".into());
                        }
                        Some(ref n) => Rc::clone(n),
                    };
                    let new_expr = self.expr_by_precedence(0)?;
                    if arg.is_none() {
                        let mut new_node = ASTnode::new_right_unary(
                            ASTop::FUNCPARAM,
                            new_expr,
                            param_node.borrow().data_type,
                        );
                        new_node.symtable_entry = Some(Rc::clone(&param_node));
                        arg = Some(new_node);
                    } else {
                        let mut new_node = ASTnode::new_boxed(
                            ASTop::FUNCPARAM,
                            arg.unwrap(),
                            new_expr,
                            param_node.borrow().data_type,
                        );
                        new_node.symtable_entry = Some(Rc::clone(&param_node));
                        arg = Some(new_node);
                    }

                    member_node = match param_node.borrow().next {
                        None => None,
                        Some(ref next) => Some(Rc::clone(next)),
                    };

                    if self.is_token_type(TokenType::COMMA)? {
                        self.consume()?;
                    }
                }
            };
        }

        self.match_token(TokenType::RPAREN)?;

        let mut funccall_node = ASTnode::new_leaf(ASTop::FUNCCALL, 0, sym.data_type);

        // Reserve left node for function IDENT
        funccall_node.right = arg;

        Ok(funccall_node)
    }
}

// How tightly this operator binds operands to its left and right
fn infix_binding_power(op: TokenType) -> Option<(u8, u8)> {
    let res = match op {
        TokenType::ASSIGN => (10, 5),
        TokenType::PLUS | TokenType::MINUS => (20, 30),
        TokenType::STAR | TokenType::SLASH => (40, 50),
        _ => return None,
    };
    Some(res)
}

fn prefix_binding_power(op: TokenType) -> Option<((), u8)> {
    let res = match op {
        TokenType::STAR => ((), 90),
        TokenType::AMPERSAND => ((), 90),
        _ => return None,
    };
    Some(res)
}

fn postfix_binding_power(op: TokenType) -> Option<(u8, ())> {
    let res = match op {
        // For function calls
        TokenType::LPAREN => (100, ()),
        TokenType::LBRACKET => (100, ()),
        TokenType::DOT => (100, ()),
        _ => return None,
    };
    Some(res)
}

fn token_type_to_binary_op(token_type: TokenType) -> ASTop {
    match token_type {
        TokenType::PLUS => ASTop::ADD,
        TokenType::MINUS => ASTop::MINUS,
        TokenType::SLASH => ASTop::DIVIDE,
        TokenType::STAR => ASTop::MULTIPLY,
        TokenType::ASSIGN => ASTop::ASSIGN,
        _ => {
            panic!("Unknown binary op from token type: {}", token_type);
        }
    }
}

fn token_type_to_postfix_op(token_type: TokenType) -> ASTop {
    match token_type {
        TokenType::DOT => ASTop::MEMBER,
        _ => {
            panic!("Unknown postfix op from token type: {}", token_type);
        }
    }
}

fn token_type_to_prefix_op(token_type: TokenType) -> ASTop {
    match token_type {
        TokenType::STAR => ASTop::DEREF,
        TokenType::AMPERSAND => ASTop::ADDR,
        _ => {
            panic!("Unknown prefix op from token type: {}", token_type);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_addition() {
        let mut scanner = Scanner::new_from_string(String::from("51+24;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.expr_by_precedence(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::ADD,
            ASTnode::new_leaf(ASTop::INTLIT, 51, DataType::INT),
            ASTnode::new_leaf(ASTop::INTLIT, 24, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_simple_addition_with_parentheses() {
        let mut scanner = Scanner::new_from_string(String::from("(51+24);"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.expr_by_precedence(0).unwrap();
        let expected = ASTnode::new_boxed(
            ASTop::ADD,
            ASTnode::new_leaf(ASTop::INTLIT, 51, DataType::INT),
            ASTnode::new_leaf(ASTop::INTLIT, 24, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_simple_addition_missing_parenthesis() {
        let mut scanner = Scanner::new_from_string(String::from("(51+24;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        match parser.expr_by_precedence(0) {
            Ok(_) => {
                panic!("Expected missing parenthesis error");
            }
            Err(r) => {
                assert_eq!(
                    r.to_string(),
                    "Syntax error, expected ')' token but ';' was found instead."
                )
            }
        }
    }

    #[test]
    fn parse_simple_addition_missing_left_operand() {
        let mut scanner = Scanner::new_from_string(String::from("+ 51;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        match parser.expr_by_precedence(0) {
            Ok(_) => {
                panic!("Expected missing left operand error");
            }
            Err(r) => {
                assert_eq!(
                    r.to_string(),
                    "Syntax error, expected primary expression but found '+' instead."
                )
            }
        }
    }
    #[test]
    fn parse_simple_addition_missing_right_operand() {
        let mut scanner = Scanner::new_from_string(String::from("51+;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        match parser.expr_by_precedence(0) {
            Ok(_) => {
                panic!("Expected missing right operand error");
            }
            Err(r) => {
                assert_eq!(
                    r.to_string(),
                    "Syntax error, expected primary expression but found ';' instead."
                )
            }
        }
    }

    #[test]
    fn parse_addition_multiplication() {
        let mut scanner = Scanner::new_from_string(String::from("1 + 2 * 3;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.expr_by_precedence(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::ADD,
            ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
            ASTnode::new_boxed(
                ASTop::MULTIPLY,
                ASTnode::new_leaf(ASTop::INTLIT, 2, DataType::INT),
                ASTnode::new_leaf(ASTop::INTLIT, 3, DataType::INT),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_division_subtraction_multiplication() {
        let mut scanner = Scanner::new_from_string(String::from("50 / 10 - 20 * 4;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.expr_by_precedence(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::MINUS,
            ASTnode::new_boxed(
                ASTop::DIVIDE,
                ASTnode::new_leaf(ASTop::INTLIT, 50, DataType::INT),
                ASTnode::new_leaf(ASTop::INTLIT, 10, DataType::INT),
                DataType::INT,
            ),
            ASTnode::new_boxed(
                ASTop::MULTIPLY,
                ASTnode::new_leaf(ASTop::INTLIT, 20, DataType::INT),
                ASTnode::new_leaf(ASTop::INTLIT, 4, DataType::INT),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_division_subtraction_multiplication_with_parentheses() {
        let mut scanner = Scanner::new_from_string(String::from("50 / (10 - 20) * 4;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.expr_by_precedence(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::MULTIPLY,
            ASTnode::new_boxed(
                ASTop::DIVIDE,
                ASTnode::new_leaf(ASTop::INTLIT, 50, DataType::INT),
                ASTnode::new_boxed(
                    ASTop::MINUS,
                    ASTnode::new_leaf(ASTop::INTLIT, 10, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 20, DataType::INT),
                    DataType::INT,
                ),
                DataType::INT,
            ),
            ASTnode::new_leaf(ASTop::INTLIT, 4, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_simple_assignment() {
        let mut scanner = Scanner::new_from_string(String::from("x = 5;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let sym = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        match expr.left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym, &s));
            }
            None => {
                panic!("ASTnode does not contain reference to symtable entry.");
            }
        }
    }

    #[test]
    fn parse_simple_char_assignment() {
        let mut scanner = Scanner::new_from_string(String::from("x = 'c';"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let sym = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_leaf(ASTop::INTLIT, 99, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        match expr.left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym, &s));
            }
            None => {
                panic!("ASTnode does not contain reference to symtable entry.");
            }
        }
    }

    #[test]
    fn parse_bin_expr_funccall_with_one_arg() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("fn_name(5 + 2)"));
        let mut parser = Parser::new(&mut scanner)?;
        let func_sym = parser.global_symbol_table.add_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            0,
            SymClass::GLOBAL,
        );
        let func_param_sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::INT,
            0,
            "param_1".to_string(),
            0,
            SymType::VARIABLE,
            SymClass::PARAM,
        );
        func_sym.borrow_mut().add_member(func_param_sym);
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::FUNCCALL,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_right_unary(
                ASTop::FUNCPARAM,
                ASTnode::new_boxed(
                    ASTop::ADD,
                    ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 2, DataType::INT),
                    DataType::INT,
                ),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        // Ensure IDENT has sym to the function
        assert!(Rc::ptr_eq(
            &func_sym,
            expr.left.as_ref().unwrap().symtable_entry.as_ref().unwrap()
        ));

        Ok(())
    }

    #[test]
    fn parse_bin_expr_funccall_with_no_arg() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("fn_name()"));
        let mut parser = Parser::new(&mut scanner)?;
        let sym = parser.global_symbol_table.add_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_unary(
            ASTop::FUNCCALL,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        // Ensure IDENT has sym to the function
        assert!(Rc::ptr_eq(
            &sym,
            expr.left.as_ref().unwrap().symtable_entry.as_ref().unwrap()
        ));

        Ok(())
    }

    #[test]
    fn parse_bin_expr_funccall_with_var_as_arg() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("fn_name(var_name)"));
        let mut parser = Parser::new(&mut scanner)?;
        let func_sym = parser.global_symbol_table.add_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            0,
            SymClass::GLOBAL,
        );
        let func_param_sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::INT,
            0,
            "param_1".to_string(),
            0,
            SymType::VARIABLE,
            SymClass::PARAM,
        );
        func_sym.borrow_mut().add_member(func_param_sym);

        let var_sym = parser.global_symbol_table.add_symbol(
            "var_name".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::FUNCCALL,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_right_unary(
                ASTop::FUNCPARAM,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        // Ensure left node has sym to the function
        assert!(Rc::ptr_eq(
            &func_sym,
            expr.left.as_ref().unwrap().symtable_entry.as_ref().unwrap()
        ));

        // Ensure right node has sym to the argument
        assert!(Rc::ptr_eq(
            &var_sym,
            expr.right
                .as_ref()
                .unwrap()
                .right
                .as_ref()
                .unwrap()
                .symtable_entry
                .as_ref()
                .unwrap()
        ));

        Ok(())
    }

    #[test]
    fn parse_bin_expr_funccall_with_two_args() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("fn_name(var_name, 2)"));
        let mut parser = Parser::new(&mut scanner)?;
        let func_sym = parser.global_symbol_table.add_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            0,
            SymClass::GLOBAL,
        );
        let func_param_sym_1 = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::INT,
            0,
            "param_1".to_string(),
            0,
            SymType::VARIABLE,
            SymClass::PARAM,
        );
        func_sym.borrow_mut().add_member(func_param_sym_1);
        let func_param_sym_2 = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::INT,
            0,
            "param_2".to_string(),
            0,
            SymType::VARIABLE,
            SymClass::PARAM,
        );
        func_sym.borrow_mut().add_member(func_param_sym_2);

        parser.global_symbol_table.add_symbol(
            "var_name".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::FUNCCALL,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_boxed(
                ASTop::FUNCPARAM,
                ASTnode::new_right_unary(
                    ASTop::FUNCPARAM,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    DataType::INT,
                ),
                ASTnode::new_leaf(ASTop::INTLIT, 2, DataType::INT),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        // Ensure left node has sym to the function
        assert!(Rc::ptr_eq(
            &func_sym,
            expr.left.as_ref().unwrap().symtable_entry.as_ref().unwrap()
        ));

        // Ensure that 1st param node has the sym for the 1st param
        assert!(Rc::ptr_eq(
            func_sym.borrow().members.as_ref().unwrap(),
            expr.right
                .as_ref()
                .unwrap()
                .left
                .as_ref()
                .unwrap()
                .symtable_entry
                .as_ref()
                .unwrap()
        ));

        // Ensure that 2nd param node has the sym for the 2nd param
        assert!(Rc::ptr_eq(
            func_sym
                .borrow()
                .members
                .as_ref()
                .unwrap()
                .borrow()
                .next
                .as_ref()
                .unwrap(),
            expr.right
                .as_ref()
                .unwrap()
                .symtable_entry
                .as_ref()
                .unwrap()
        ));

        Ok(())
    }

    #[test]
    fn parse_bin_expr_with_prefix_deref_assign() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("*x = 8;"));
        let mut parser = Parser::new(&mut scanner)?;
        let sym = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INTPTR,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_unary(
                ASTop::DEREF,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INTPTR),
                DataType::INT,
            ),
            ASTnode::new_leaf(ASTop::INTLIT, 8, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        match expr.left.unwrap().left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym, &s));
            }
            None => {
                panic!("ASTnode does not contain reference to symtable entry.");
            }
        };
        Ok(())
    }

    #[test]
    fn parse_bin_expr_with_deref_as_rvalue() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("x = *y;"));
        let mut parser = Parser::new(&mut scanner)?;
        let sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let sym_y = parser.global_symbol_table.add_symbol(
            "y".to_string(),
            DataType::INTPTR,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_unary(
                ASTop::DEREF,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INTPTR),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        match expr.left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym_x, &s));
            }
            None => {
                panic!("Left ASTnode does not contain reference to symtable entry.");
            }
        };

        match expr.right.unwrap().left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym_y, &s));
            }
            None => {
                panic!("Right ASTnode does not contain reference to symtable entry.");
            }
        };
        Ok(())
    }

    #[test]
    fn parse_bin_expr_with_address_taking_of_variable() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("x = &y;"));
        let mut parser = Parser::new(&mut scanner)?;
        let sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INTPTR,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let sym_y = parser.global_symbol_table.add_symbol(
            "y".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INTPTR),
            ASTnode::new_unary(
                ASTop::ADDR,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                DataType::INTPTR,
            ),
            DataType::INTPTR,
        );

        match_ast_node(Some(&expr), expected);

        match expr.left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym_x, &s));
            }
            None => {
                panic!("Left ASTnode does not contain reference to symtable entry.");
            }
        };

        match expr.right.unwrap().left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym_y, &s));
            }
            None => {
                panic!("Right ASTnode does not contain reference to symtable entry.");
            }
        };

        Ok(())
    }

    #[test]
    fn parse_assignation_to_struct_member() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("person1.x = 5;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let struct_sym = parser.composite_symbol_table.add_symbol(
            "Person".to_string(),
            DataType::NONE,
            0,
            SymType::STRUCT,
            0,
            SymClass::STRUCT,
        );

        let struct_member_sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::INT,
            0,
            String::from("x"),
            0,
            SymType::VARIABLE,
            SymClass::MEMBER,
        );
        struct_sym.borrow_mut().add_member(struct_member_sym);

        let sym_person1 = parser.global_symbol_table.add_symbol(
            "person1".to_string(),
            DataType::STRUCT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        sym_person1.borrow_mut().type_sym = Some(Rc::clone(&struct_sym));

        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_unary(
                ASTop::MEMBER,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::STRUCT),
                DataType::INT,
            ),
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        let member_node = expr.left.unwrap();

        match member_node.symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(
                    struct_sym.borrow().members.as_ref().unwrap(),
                    &s
                ));
            }
            None => {
                panic!("Left ASTnode does not contain reference to symtable entry.");
            }
        };

        match member_node.left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym_person1, &s));
            }
            None => {
                panic!("Left ASTnode does not contain reference to symtable entry.");
            }
        };

        Ok(())
    }

    #[test]
    fn parse_access_struct_member_as_rvalue() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("y = person1.x;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let struct_sym = parser.composite_symbol_table.add_symbol(
            "Person".to_string(),
            DataType::NONE,
            0,
            SymType::STRUCT,
            0,
            SymClass::STRUCT,
        );

        let struct_member_sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::INT,
            0,
            String::from("x"),
            0,
            SymType::VARIABLE,
            SymClass::MEMBER,
        );
        struct_sym.borrow_mut().add_member(struct_member_sym);

        let _sym_y = parser.global_symbol_table.add_symbol(
            "y".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );

        let sym_person1 = parser.global_symbol_table.add_symbol(
            "person1".to_string(),
            DataType::STRUCT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        sym_person1.borrow_mut().type_sym = Some(Rc::clone(&struct_sym));

        let expr = parser.expr_by_precedence(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_unary(
                ASTop::MEMBER,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::STRUCT),
                DataType::INT,
            ),
            DataType::INT,
        );

        match_ast_node(Some(&expr), expected);

        let member_node = expr.right.unwrap();

        match member_node.symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(
                    struct_sym.borrow().members.as_ref().unwrap(),
                    &s
                ));
            }
            None => {
                panic!("Left ASTnode does not contain reference to symtable entry.");
            }
        };

        match member_node.left.unwrap().symtable_entry {
            Some(s) => {
                assert!(Rc::ptr_eq(&sym_person1, &s));
            }
            None => {
                panic!("Left ASTnode does not contain reference to symtable entry.");
            }
        };

        Ok(())
    }
}
