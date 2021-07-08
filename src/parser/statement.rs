// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use crate::code_generation::generate_label_for_function;
use std::convert::TryFrom;

impl<'a> Parser<'a> {
    pub fn parse_global_declarations(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let mut tree: Option<Box<ASTnode>> = None;

        while self.current_token.as_ref().unwrap().token_type != TokenType::EOF {
            let node = self.parse_global_declaration()?;

            if let Some(node) = node {
                match tree {
                    Some(original_tree) => {
                        tree = Some(ASTnode::new_boxed(ASTop::GLUE, original_tree, node))
                    }
                    None => {
                        tree = Some(node);
                    }
                }
            }
        }

        match tree {
            Some(tree) => Ok(tree),
            None => Err(format!("Empty program has been found.").into()),
        }
    }

    // TODO: Turn this function into parse_global_declaration to
    // eventually only handle var and func declarations
    pub fn parse_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let statement = match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::PRINT => self.print_statement()?,
                TokenType::RETURN => self.parse_return()?,
                _ => {
                    let node = self.binary_expr(0)?;
                    self.match_token(TokenType::SEMI)?;
                    node
                }
            },
            None => {
                panic!("Unexpected error: No more tokens found in parse_statement.");
            }
        };

        Ok(statement)
    }

    // Parse a global declaration and maybe return an ASTnode.
    // Global var declarations do not yield ASTNodes
    fn parse_global_declaration(&mut self) -> Result<Option<Box<ASTnode>>, Box<dyn Error>> {
        self.match_token(TokenType::INT)?;
        let ident = self.match_token(TokenType::IDENT)?;

        match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::LPAREN => {
                    let tree = self.parse_func_declaration(ident)?;
                    Ok(Some(tree))
                }
                TokenType::SEMI | TokenType::COMMA => {
                    self.parse_global_var_declaration(TokenType::INT, ident)?;
                    Ok(None)
                }
                _ => Err(format!("Syntax error, unexpected '{}' found", tok.token_type).into()),
            },
            None => {
                panic!("Unexpected error: No more tokens found in parse_statement.");
            }
        }
    }

    // We require these parameters as these tokens should have
    // been scanned prior to invocation of this function
    fn parse_global_var_declaration(
        &mut self,
        data_type: TokenType,
        ident: Token,
    ) -> Result<(), Box<dyn Error>> {
        // TODO: Allow assignation of initial value during declaration

        self.add_global_symbol(
            ident.lexeme,
            DataType::try_from(data_type)?,
            0,
            SymType::VARIABLE,
            4,
        );

        match &self.current_token {
            Some(token) => {
                match token.token_type {
                    TokenType::SEMI => {
                        self.consume()?;
                    }
                    TokenType::COMMA => {
                        // If there are more declarations to parse, keep going
                        self.consume()?;
                        let ident_tok = self.match_token(TokenType::IDENT)?;
                        self.parse_global_var_declaration(data_type, ident_tok)?;
                    }
                    _ => return Err("Missing ',' or ';' after declaration".into()),
                }
            }
            None => return Err("Missing ',' or ';' after declaration".into()),
        }

        Ok(())
    }

    // Parse param list in function
    // We require these parameters as these tokens should have
    // been scanned prior to invocation of this function
    fn parse_func_param_declaration(&mut self) -> Result<SymbolTableEntry, Box<dyn Error>> {
        self.match_token(TokenType::INT)?;
        let ident = self.match_token(TokenType::IDENT)?;

        let mut sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            DataType::try_from(TokenType::INT)?,
            0,
            ident.lexeme.clone(),
            4,
            SymType::VARIABLE,
            SymClass::PARAM,
        );

        match &self.current_token {
            Some(token) => {
                match token.token_type {
                    TokenType::RPAREN => {
                        // Let caller consume the RPAREN
                    }
                    TokenType::COMMA => {
                        // If there are more declarations to parse, keep going
                        self.consume()?;
                        let next_sym = self.parse_func_param_declaration()?;
                        sym.add_to_list(next_sym);
                    }
                    _ => return Err("Missing ',' or ';' after declaration".into()),
                }
            }
            None => return Err("Missing ',' or ';' after declaration".into()),
        }

        Ok(sym)
    }

    fn parse_func_declaration(&mut self, ident: Token) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let sym = self.add_global_symbol(ident.lexeme, DataType::INT, 0, SymType::FUNCTION, 0);

        // TODO: Support defining parameters
        self.match_token(TokenType::LPAREN)?;

        if !self.is_token_type(TokenType::RPAREN)? {
            sym.borrow_mut()
                .add_member(self.parse_func_param_declaration()?);
        }

        self.match_token(TokenType::RPAREN)?;
        self.match_token(TokenType::LBRACE)?;

        let mut tree: Option<Box<ASTnode>> = None;

        // TODO: Figure out the right return type
        // We add the symbol here so that recursive calls work

        // Point to the symbol of the function we are parsing so that it can be referenced
        // when we parse the body of the function.
        self.current_func_sym = Some(Rc::clone(&sym));

        // TODO: Is there a better way to track whether or not a return was included?
        let mut last_op = ASTop::NOOP;

        while self.current_token.as_ref().unwrap().token_type != TokenType::RBRACE {
            let node = self.parse_statement()?;
            last_op = node.op;

            match tree {
                Some(original_tree) => {
                    tree = Some(ASTnode::new_boxed(ASTop::GLUE, original_tree, node))
                }
                None => {
                    tree = Some(node);
                }
            }
        }

        // TODO: Ensure that return statement is included for non-void functions.
        // Alternately, we can allow this and emit a warning
        if last_op != ASTop::RETURN {
            return Err("Return value must be provided in non-void function.".into());
        }

        // Since we are done parsing the body of the function, we remove the pointer
        // to the symbol of the current function.
        self.current_func_sym = None;

        self.match_token(TokenType::RBRACE)?;

        let mut fn_node = ASTnode::new_unary(
            ASTop::FUNCTION,
            tree.unwrap_or_else(|| ASTnode::new_noop()), // TODO: Make this return ASTop::RETURN by default
        );

        fn_node.symtable_entry = Some(sym);

        Ok(fn_node)
    }

    fn parse_return(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        if self.current_func_sym.is_none() {
            return Err(format!("Cannot return from outside a function.").into());
        }
        self.match_token(TokenType::RETURN)?;
        let binary_node = self.binary_expr(0)?;
        self.match_token(TokenType::SEMI)?;

        // TODO: Ensure that returning values is not allowed in void
        // functions.
        let mut ret_node = ASTnode::new_unary(
            ASTop::RETURN,
            binary_node, // TODO: Make this return ASTop::RETURN by default
        );

        // Unwrap since we already verified that this is not none at the beginning of the function
        ret_node.label = Some(generate_label_for_function(
            &self.current_func_sym.as_ref().unwrap().borrow(),
        ));

        Ok(ret_node)
    }

    pub fn print_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        self.match_token(TokenType::PRINT)?;
        self.match_token(TokenType::LPAREN)?;
        let binary_node = self.binary_expr(0)?;
        self.match_token(TokenType::RPAREN)?;
        self.match_token(TokenType::SEMI)?;

        Ok(ASTnode::new_unary(ASTop::PRINT, binary_node))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_print_statement() {
        let mut scanner = Scanner::new_from_string(String::from("print(51+24);"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.print_statement().unwrap();
        let expected = ASTnode::new_unary(
            ASTop::PRINT,
            ASTnode::new_boxed(
                ASTop::ADD,
                ASTnode::new_leaf(ASTop::INTLIT, 51),
                ASTnode::new_leaf(ASTop::INTLIT, 24),
            ),
        );

        match_ast_node(Some(&expr), expected);
    }

    #[test]
    fn parse_global_single_var_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int x;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap();

        assert!(expr.is_none());

        match parser.global_symbol_table.get("x") {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::VARIABLE);
            }
            None => panic!("New symbol table entry not found"),
        }
    }

    #[test]
    fn parse_global_var_list_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int x, y;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap();

        assert!(expr.is_none());

        match parser.global_symbol_table.get("x") {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::VARIABLE);
            }
            None => panic!("New symbol table entry for x was not found"),
        }

        match parser.global_symbol_table.get("y") {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::VARIABLE);
            }
            None => panic!("New symbol table entry for y was not found"),
        }
    }

    #[test]
    fn parse_func_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int main() { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap().unwrap();

        let expected = ASTnode::new_unary(
            ASTop::FUNCTION,
            ASTnode::new_unary(ASTop::RETURN, ASTnode::new_leaf(ASTop::INTLIT, 1)),
        );
        match_ast_node(Some(&expr), expected);
        match parser.global_symbol_table.get("main") {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::FUNCTION);
                assert_eq!(new_sym.sym_class, SymClass::GLOBAL);
            }
            None => panic!("New symbol table entry not found"),
        }
    }

    #[test]
    fn parse_func_declaration_no_ret() {
        let mut scanner = Scanner::new_from_string(String::from("int main() {}"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration();

        match expr {
            Ok(_) => {
                panic!("Did not receive expected error for omitting return statement.");
            }
            Err(e) => {
                assert_eq!(
                    e.to_string(),
                    "Return value must be provided in non-void function."
                );
            }
        }
    }

    #[test]
    fn parse_func_declaration_with_one_param() {
        let mut scanner = Scanner::new_from_string(String::from("int test(int x) { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap().unwrap();

        let expected = ASTnode::new_unary(
            ASTop::FUNCTION,
            ASTnode::new_unary(ASTop::RETURN, ASTnode::new_leaf(ASTop::INTLIT, 1)),
        );
        match_ast_node(Some(&expr), expected);
        match parser.global_symbol_table.get("test") {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::FUNCTION);
                assert_eq!(new_sym.sym_class, SymClass::GLOBAL);

                let param_sym = new_sym.members.as_ref().unwrap().borrow();
                assert_eq!(param_sym.data_type, DataType::INT);
                assert_eq!(param_sym.initial_value, 0);
                assert_eq!(param_sym.sym_type, SymType::VARIABLE);
                assert_eq!(param_sym.sym_class, SymClass::PARAM);
                assert_eq!(param_sym.name, "x");
            }
            None => panic!("New symbol table entry not found"),
        }
    }

    #[test]
    fn parse_func_declaration_with_two_params() {
        let mut scanner =
            Scanner::new_from_string(String::from("int test(int x, int y) { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap().unwrap();

        let expected = ASTnode::new_unary(
            ASTop::FUNCTION,
            ASTnode::new_unary(ASTop::RETURN, ASTnode::new_leaf(ASTop::INTLIT, 1)),
        );
        match_ast_node(Some(&expr), expected);
        match parser.global_symbol_table.get("test") {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::FUNCTION);
                assert_eq!(new_sym.sym_class, SymClass::GLOBAL);

                let param_sym_1 = new_sym.members.as_ref().unwrap().borrow();
                assert_eq!(param_sym_1.data_type, DataType::INT);
                assert_eq!(param_sym_1.initial_value, 0);
                assert_eq!(param_sym_1.sym_type, SymType::VARIABLE);
                assert_eq!(param_sym_1.sym_class, SymClass::PARAM);
                assert_eq!(param_sym_1.name, "x");

                let param_sym_2 = param_sym_1.next.as_ref().unwrap().borrow();
                assert_eq!(param_sym_2.data_type, DataType::INT);
                assert_eq!(param_sym_2.initial_value, 0);
                assert_eq!(param_sym_2.sym_type, SymType::VARIABLE);
                assert_eq!(param_sym_2.sym_class, SymClass::PARAM);
                assert_eq!(param_sym_2.name, "y");
            }
            None => panic!("New symbol table entry not found"),
        }
    }
}
