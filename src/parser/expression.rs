// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use std::error::Error;

impl<'a> Parser<'a> {
    // Pratt parser inspired by https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    pub fn binary_expr(&mut self, min_bp: u8) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let mut left = self.primary_expr()?;

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

                // Handle function calls
                if op_tok_type == TokenType::LPAREN {
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
                } else {
                    self.consume()?;

                    left = ASTnode::new_unary(token_type_to_postfix_op(op_tok_type), left);
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

                let mut right = self.binary_expr(r_bp)?;

                if op_tok_type == TokenType::ASSIGN {
                    right.rvalue = true;
                } else {
                    left.rvalue = true;
                    right.rvalue = true;
                }

                left = ASTnode::new_boxed(token_type_to_binary_op(op_tok_type), left, right);
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
                    let expr = self.binary_expr(0)?;
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
                        symtable_entry = match self.global_symbol_table.get(&token.lexeme) {
                            Some(new_sym) => Some(Rc::clone(new_sym)),
                            None => {
                                return Err(format!(
                                    "Referencing undefined variable {}.",
                                    &token.lexeme
                                )
                                .into())
                            }
                        };
                    }

                    let mut node = ASTnode::new_leaf(ASTop::IDENT, 0);
                    node.symtable_entry = Some(Rc::clone(symtable_entry.as_ref().unwrap()));

                    Ok(node)
                }
                TokenType::INTLIT => Ok(ASTnode::new_leaf(
                    ASTop::INTLIT,
                    self.match_token(TokenType::INTLIT)?.int_value,
                )),
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
        // sym: &Rc<SymbolTableEntry>,
    ) -> Result<Box<ASTnode>, Box<dyn Error>> {
        match sym.sym_type {
            SymType::FUNCTION => {}
            _ => {
                return Err(format!("Call to {} which is not a function.", &sym.name).into());
            }
        };

        self.match_token(TokenType::LPAREN)?;
        let mut arg = None;

        match self.current_token.as_ref().unwrap().token_type {
            TokenType::RPAREN => {}
            _ => {
                // TODO: Here we are only accepting one argument
                arg = Some(self.binary_expr(0)?);
            }
        };

        self.match_token(TokenType::RPAREN)?;

        let mut funccall_node = ASTnode::new_leaf(ASTop::FUNCCALL, 0);

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

fn postfix_binding_power(op: TokenType) -> Option<(u8, ())> {
    let res = match op {
        // For function calls
        TokenType::LPAREN => (100, ()),
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
        _ => {
            panic!("Unknown postfix op from token type: {}", token_type);
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
        let expr = parser.binary_expr(0).unwrap();
        let expected = ASTnode::new_boxed(
            ASTop::ADD,
            ASTnode::new_leaf(ASTop::INTLIT, 51),
            ASTnode::new_leaf(ASTop::INTLIT, 24),
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_simple_addition_with_parentheses() {
        let mut scanner = Scanner::new_from_string(String::from("(51+24);"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.binary_expr(0).unwrap();
        let expected = ASTnode::new_boxed(
            ASTop::ADD,
            ASTnode::new_leaf(ASTop::INTLIT, 51),
            ASTnode::new_leaf(ASTop::INTLIT, 24),
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_simple_addition_missing_parenthesis() {
        let mut scanner = Scanner::new_from_string(String::from("(51+24;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        match parser.binary_expr(0) {
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
        match parser.binary_expr(0) {
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
        match parser.binary_expr(0) {
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
        let expr = parser.binary_expr(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::ADD,
            ASTnode::new_leaf(ASTop::INTLIT, 1),
            ASTnode::new_boxed(
                ASTop::MULTIPLY,
                ASTnode::new_leaf(ASTop::INTLIT, 2),
                ASTnode::new_leaf(ASTop::INTLIT, 3),
            ),
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_division_subtraction_multiplication() {
        let mut scanner = Scanner::new_from_string(String::from("50 / 10 - 20 * 4;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.binary_expr(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::MINUS,
            ASTnode::new_boxed(
                ASTop::DIVIDE,
                ASTnode::new_leaf(ASTop::INTLIT, 50),
                ASTnode::new_leaf(ASTop::INTLIT, 10),
            ),
            ASTnode::new_boxed(
                ASTop::MULTIPLY,
                ASTnode::new_leaf(ASTop::INTLIT, 20),
                ASTnode::new_leaf(ASTop::INTLIT, 4),
            ),
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_division_subtraction_multiplication_with_parentheses() {
        let mut scanner = Scanner::new_from_string(String::from("50 / (10 - 20) * 4;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.binary_expr(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::MULTIPLY,
            ASTnode::new_boxed(
                ASTop::DIVIDE,
                ASTnode::new_leaf(ASTop::INTLIT, 50),
                ASTnode::new_boxed(
                    ASTop::MINUS,
                    ASTnode::new_leaf(ASTop::INTLIT, 10),
                    ASTnode::new_leaf(ASTop::INTLIT, 20),
                ),
            ),
            ASTnode::new_leaf(ASTop::INTLIT, 4),
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_simple_assignment() {
        let mut scanner = Scanner::new_from_string(String::from("x = 5;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let sym = parser.add_global_symbol("x".to_string(), DataType::INT, 0, SymType::VARIABLE, 4);
        let expr = parser.binary_expr(0).unwrap();

        let expected = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0),
            ASTnode::new_leaf(ASTop::INTLIT, 5),
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
        let sym = parser.add_global_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            4,
        );
        let expr = parser.binary_expr(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::FUNCCALL,
            ASTnode::new_leaf(ASTop::IDENT, 0),
            ASTnode::new_boxed(
                ASTop::ADD,
                ASTnode::new_leaf(ASTop::INTLIT, 5),
                ASTnode::new_leaf(ASTop::INTLIT, 2),
            ),
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
    fn parse_bin_expr_funccall_with_no_arg() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("fn_name()"));
        let mut parser = Parser::new(&mut scanner)?;
        let sym = parser.add_global_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            4,
        );
        let expr = parser.binary_expr(0)?;

        let expected = ASTnode::new_unary(ASTop::FUNCCALL, ASTnode::new_leaf(ASTop::IDENT, 0));

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
        let func_sym = parser.add_global_symbol(
            "fn_name".to_string(),
            DataType::INT,
            0,
            SymType::FUNCTION,
            0,
        );
        let var_sym = parser.add_global_symbol(
            "var_name".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            4,
        );
        let expr = parser.binary_expr(0)?;

        let expected = ASTnode::new_boxed(
            ASTop::FUNCCALL,
            ASTnode::new_leaf(ASTop::IDENT, 0),
            ASTnode::new_leaf(ASTop::IDENT, 0),
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
                .symtable_entry
                .as_ref()
                .unwrap()
        ));

        Ok(())
    }
}
