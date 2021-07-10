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
                        tree = Some(ASTnode::new_boxed(
                            ASTop::GLUE,
                            original_tree,
                            node,
                            DataType::NONE,
                        ))
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
                TokenType::RETURN => self.parse_return()?,
                _ => {
                    let node = self.expr_by_precedence(0)?;
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
        let data_type = DataType::try_from(self.parse_type()?)?;
        let ident = self.match_token(TokenType::IDENT)?;

        match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::LPAREN => {
                    let tree = self.parse_func_declaration(ident, data_type)?;
                    Ok(Some(tree))
                }
                TokenType::SEMI | TokenType::COMMA => {
                    self.parse_global_var_declaration(data_type, ident)?;
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
        data_type: DataType,
        ident: Token,
    ) -> Result<(), Box<dyn Error>> {
        // TODO: Allow assignation of initial value during declaration

        if data_type == DataType::VOID {
            return Err("Unable to declare variables with void type.".into());
        }

        self.add_global_symbol(ident.lexeme, data_type, 0, SymType::VARIABLE, 4);

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
        let data_type = DataType::try_from(self.parse_type()?)?;

        if data_type == DataType::VOID {
            return Err("Unable to define function parameters with void type.".into());
        }

        let ident = self.match_token(TokenType::IDENT)?;

        let mut sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            data_type,
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

    fn parse_func_declaration(
        &mut self,
        ident: Token,
        data_type: DataType,
    ) -> Result<Box<ASTnode>, Box<dyn Error>> {
        // We add the symbol here so that recursive calls work
        let sym = self.add_global_symbol(ident.lexeme, data_type, 0, SymType::FUNCTION, 0);

        // TODO: Support defining parameters
        self.match_token(TokenType::LPAREN)?;

        if !self.is_token_type(TokenType::RPAREN)? {
            sym.borrow_mut()
                .add_member(self.parse_func_param_declaration()?);
        }

        self.match_token(TokenType::RPAREN)?;
        self.match_token(TokenType::LBRACE)?;

        let mut tree: Option<Box<ASTnode>> = None;

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
                    tree = Some(ASTnode::new_boxed(
                        ASTop::GLUE,
                        original_tree,
                        node,
                        DataType::NONE,
                    ))
                }
                None => {
                    tree = Some(node);
                }
            }
        }

        // TODO: Eventually we have to check if return statements were included
        // in different branches of control flow statements, i.e. return statements
        // will not always be last statement within a function
        if data_type != DataType::VOID && last_op != ASTop::RETURN {
            return Err("Return value must be provided in non-void function.".into());
        }

        // Since we are done parsing the body of the function, we remove the pointer
        // to the symbol of the current function.
        self.current_func_sym = None;

        self.match_token(TokenType::RBRACE)?;

        let mut fn_node = ASTnode::new_unary(
            ASTop::FUNCTION,
            tree.unwrap_or_else(|| ASTnode::new_noop()),
            DataType::NONE,
        );

        fn_node.symtable_entry = Some(sym);

        Ok(fn_node)
    }

    fn parse_return(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let ret_type = match &self.current_func_sym {
            Some(fn_sym) => fn_sym.borrow().data_type,
            None => {
                return Err(format!("Cannot return from outside a function.").into());
            }
        };

        self.match_token(TokenType::RETURN)?;

        let mut ret_node = match self.is_token_type(TokenType::SEMI)? {
            true => {
                if ret_type != DataType::VOID {
                    return Err(
                        "Empty return statements are not allowed in non-void functions.".into(),
                    );
                }
                ASTnode::new_leaf(ASTop::RETURN, 0, DataType::NONE)
            }
            false => {
                if ret_type == DataType::VOID {
                    return Err("Unable to return values from void functions.".into());
                }
                let binary_node = self.expr_by_precedence(0)?;
                ASTnode::new_unary(ASTop::RETURN, binary_node, DataType::NONE)
            }
        };

        self.match_token(TokenType::SEMI)?;

        // Safe to unwrap since the presence of current_func_sym has been verifed before
        ret_node.label = Some(generate_label_for_function(
            &self.current_func_sym.as_ref().unwrap().borrow(),
        ));

        Ok(ret_node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn parse_global_void_var_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("void x;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration();

        match expr {
            Ok(_) => {
                panic!("Declaration of global variable with void type should have failed");
            }
            Err(e) => {
                assert_eq!("Unable to declare variables with void type.", e.to_string());
            }
        }
    }

    #[test]
    fn parse_func_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int main() { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap().unwrap();

        let expected = ASTnode::new_unary(
            ASTop::FUNCTION,
            ASTnode::new_unary(
                ASTop::RETURN,
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::CHAR),
                DataType::NONE,
            ),
            DataType::NONE,
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
            ASTnode::new_unary(
                ASTop::RETURN,
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::CHAR),
                DataType::NONE,
            ),
            DataType::NONE,
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
            ASTnode::new_unary(
                ASTop::RETURN,
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::CHAR),
                DataType::NONE,
            ),
            DataType::NONE,
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

    #[test]
    fn parse_func_declaration_with_void_param() {
        let mut scanner = Scanner::new_from_string(String::from("int test(void x) { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration();

        match expr {
            Ok(_) => {
                panic!("Parsing of function with void parameter should have failed.");
            }

            Err(e) => {
                assert_eq!(
                    e.to_string(),
                    "Unable to define function parameters with void type."
                );
            }
        }
    }
}
