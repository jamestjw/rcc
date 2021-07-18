// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use std::convert::TryInto;

use super::*;
use crate::code_generation::generate_label_for_function;

impl<'a> Parser<'a> {
    pub fn parse_global_statements(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
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
        match self.current_token_type()? {
            TokenType::STRUCT => {
                self.parse_struct_definition()?;
                Ok(None)
            }
            _ => {
                let data_type = self.parse_type()?;
                let ident = self.match_token(TokenType::IDENT)?;

                match &self.current_token {
                    Some(tok) => match tok.token_type {
                        TokenType::LPAREN => {
                            let tree = self.parse_func_declaration(ident, data_type)?;
                            Ok(Some(tree))
                        }
                        TokenType::SEMI | TokenType::COMMA => {
                            self.parse_global_scalar_declaration(data_type, ident)?;
                            Ok(None)
                        }
                        TokenType::LBRACKET => {
                            self.parse_global_array_declaration(data_type, ident)?;
                            Ok(None)
                        }
                        _ => Err(
                            format!("Syntax error, unexpected '{}' found", tok.token_type).into(),
                        ),
                    },
                    None => {
                        panic!("Unexpected error: No more tokens found in parse_statement.");
                    }
                }
            }
        }
    }

    // We require these parameters as these tokens should have
    // been scanned prior to invocation of this function
    fn parse_global_scalar_declaration(
        &mut self,
        data_type: DataType,
        ident: Token,
    ) -> Result<(), Box<dyn Error>> {
        // TODO: Allow assignation of initial value during declaration

        if data_type == DataType::VOID {
            return Err("Unable to declare variables with void type.".into());
        }

        self.global_symbol_table.add_symbol(
            ident.lexeme,
            data_type,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
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
                        self.parse_global_scalar_declaration(data_type, ident_tok)?;
                    }
                    _ => return Err("Missing ',' or ';' after declaration".into()),
                }
            }
            None => return Err("Missing ',' or ';' after declaration".into()),
        }

        Ok(())
    }

    fn parse_global_array_declaration(
        &mut self,
        data_type: DataType,
        ident: Token,
    ) -> Result<(), Box<dyn Error>> {
        // TODO: Allow assignation of initial value during declaration

        if data_type == DataType::VOID {
            return Err("Unable to declare array with void type.".into());
        }
        self.match_token(TokenType::LBRACKET)?;
        let array_size: u16 = self
            .match_token(TokenType::INTLIT)?
            .int_value
            .try_into()
            .expect("Array size is too large.");
        self.match_token(TokenType::RBRACKET)?;

        self.global_symbol_table.add_symbol(
            ident.lexeme,
            to_pointer(data_type)?,
            0,
            SymType::ARRAY(array_size),
            0,
            SymClass::GLOBAL,
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
                        self.parse_global_array_declaration(data_type, ident_tok)?;
                    }
                    _ => return Err("Missing ',' or ';' after array declaration".into()),
                }
            }
            None => return Err("Missing ',' or ';' after array declaration".into()),
        }

        Ok(())
    }

    // Parse param list in function
    // We require these parameters as these tokens should have
    // been scanned prior to invocation of this function
    fn parse_func_param_declaration(&mut self) -> Result<SymbolTableEntry, Box<dyn Error>> {
        let data_type = self.parse_type()?;

        if data_type == DataType::VOID {
            return Err("Unable to define function parameters with void type.".into());
        }

        let ident = self.match_token(TokenType::IDENT)?;

        let mut sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            data_type,
            0,
            ident.lexeme.clone(),
            0,
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

    // Handles all three possibilities
    // i. struct Person { int x; };
    // ii. struct Person { int x; } person1;
    // iii. struct Person person1;
    fn parse_struct_definition(&mut self) -> Result<(), Box<dyn Error>> {
        self.match_token(TokenType::STRUCT)?;
        let struct_name_tok = self.match_token(TokenType::IDENT)?;

        let next_token = self.current_token_type()?;

        match next_token {
            // Creating variable from previously defined struct
            TokenType::IDENT => {
                self.parse_struct_variable_declaration(&struct_name_tok.lexeme)?;
                self.match_token(TokenType::SEMI)?;
                Ok(())
            }
            TokenType::LBRACE => {
                // So that we can handle references to the same struct within its definition
                let sym = self.composite_symbol_table.add_symbol(
                    struct_name_tok.lexeme.clone(),
                    DataType::NONE,
                    0,
                    SymType::STRUCT,
                    0,
                    SymClass::STRUCT,
                );

                self.match_token(TokenType::LBRACE)?;

                // Only parse members if the struct is not empty
                if !self.is_token_type(TokenType::RBRACE)? {
                    sym.borrow_mut()
                        .add_member(self.parse_struct_members_declaration()?);
                }

                self.match_token(TokenType::RBRACE)?;

                // We might have a struct variable definition following definition of a struct.
                let next_token = self.current_token_type()?;

                match next_token {
                    TokenType::SEMI => {}
                    TokenType::IDENT => {
                        self.parse_struct_variable_declaration(&struct_name_tok.lexeme)?;
                    }
                    _ => {
                        return Err(format!(
                            "Syntax error, expected ';' or identifier but encountered {} instead",
                            next_token
                        )
                        .into());
                    }
                }

                self.match_token(TokenType::SEMI)?;
                Ok(())
            }
            _ => {
                return Err(format!(
                    "Syntax error, expected '{{' or identifier but encountered {} instead",
                    next_token
                )
                .into());
            }
        }
    }

    fn parse_struct_variable_declaration(
        &mut self,
        struct_name: &str,
    ) -> Result<(), Box<dyn Error>> {
        let ident = self.match_token(TokenType::IDENT)?;
        match self
            .composite_symbol_table
            .find_symbol(struct_name, SymClass::STRUCT)
        {
            Some(struct_sym) => {
                // We add the symbol here so that recursive calls work
                let sym = self.global_symbol_table.add_symbol(
                    ident.lexeme,
                    DataType::STRUCT,
                    0,
                    SymType::VARIABLE,
                    0,
                    SymClass::GLOBAL,
                );
                sym.borrow_mut().type_sym = Some(Rc::clone(&struct_sym));

                Ok(())
            }
            None => Err(format!("Referencing undefined struct: {}", &ident.lexeme).into()),
        }
    }

    fn parse_struct_members_declaration(&mut self) -> Result<SymbolTableEntry, Box<dyn Error>> {
        // TODO: Support nested structs
        // TODO: Support arrays in structs
        let data_type = self.parse_type()?;

        if data_type == DataType::VOID {
            return Err("Unable to define struct members with void type.".into());
        }

        let ident = self.match_token(TokenType::IDENT)?;

        let mut sym = SymbolTableEntry::new(
            // TODO: Parse datatype
            data_type,
            0,
            ident.lexeme.clone(),
            0,
            SymType::VARIABLE,
            SymClass::MEMBER,
        );

        self.match_token(TokenType::SEMI)?;

        match &self.current_token {
            Some(token) => {
                match token.token_type {
                    TokenType::RBRACE => {
                        // Let caller consume the RBRACE
                    }
                    _ => {
                        // If we hit did not hit a RBRACE, we assume that there are more members
                        let next_sym = self.parse_struct_members_declaration()?;
                        sym.add_to_list(next_sym);
                    }
                }
            }
            None => return Err("Missing '}' after struct declaration".into()),
        }

        Ok(sym)
    }

    fn parse_func_declaration(
        &mut self,
        ident: Token,
        data_type: DataType,
    ) -> Result<Box<ASTnode>, Box<dyn Error>> {
        // We add the symbol here so that recursive calls work
        let sym = self.global_symbol_table.add_symbol(
            ident.lexeme,
            data_type,
            0,
            SymType::FUNCTION,
            0,
            SymClass::GLOBAL,
        );

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

        match parser
            .global_symbol_table
            .find_symbol("x", SymClass::GLOBAL)
        {
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

        match parser
            .global_symbol_table
            .find_symbol("x", SymClass::GLOBAL)
        {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::VARIABLE);
            }
            None => panic!("New symbol table entry for x was not found"),
        }

        match parser
            .global_symbol_table
            .find_symbol("y", SymClass::GLOBAL)
        {
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
    fn parse_global_single_array_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int x[5];"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap();

        assert!(expr.is_none());

        match parser
            .global_symbol_table
            .find_symbol("x", SymClass::GLOBAL)
        {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INTPTR);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::ARRAY(5));
            }
            None => panic!("New symbol table entry not found"),
        }
    }

    #[test]
    fn parse_global_array_list_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int x[5], y[10];"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap();

        assert!(expr.is_none());

        match parser
            .global_symbol_table
            .find_symbol("x", SymClass::GLOBAL)
        {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INTPTR);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::ARRAY(5));
            }
            None => panic!("New symbol table entry not found for x"),
        }

        match parser
            .global_symbol_table
            .find_symbol("y", SymClass::GLOBAL)
        {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INTPTR);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::ARRAY(10));
            }
            None => panic!("New symbol table entry not found for y"),
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
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
                DataType::NONE,
            ),
            DataType::NONE,
        );
        match_ast_node(Some(&expr), expected);
        match parser
            .global_symbol_table
            .find_symbol("main", SymClass::GLOBAL)
        {
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
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
                DataType::NONE,
            ),
            DataType::NONE,
        );
        match_ast_node(Some(&expr), expected);
        match parser
            .global_symbol_table
            .find_symbol("test", SymClass::GLOBAL)
        {
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
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
                DataType::NONE,
            ),
            DataType::NONE,
        );
        match_ast_node(Some(&expr), expected);
        match parser
            .global_symbol_table
            .find_symbol("test", SymClass::GLOBAL)
        {
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

    #[test]
    fn parse_struct_definition() {
        let mut scanner = Scanner::new_from_string(String::from("struct Person { int x; };"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        parser
            .parse_global_declaration()
            .expect("Failed to define struct");

        match parser
            .composite_symbol_table
            .find_symbol("Person", SymClass::STRUCT)
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.name, "Person");
                assert_eq!(sym.sym_type, SymType::STRUCT);
                assert_eq!(sym.sym_class, SymClass::STRUCT);
                match &sym.members {
                    Some(member) => {
                        let member = member.borrow();
                        assert_eq!(member.name, "x");
                        assert_eq!(member.sym_type, SymType::VARIABLE);
                        assert_eq!(member.sym_class, SymClass::MEMBER);
                    }
                    None => {
                        panic!("Struct member was not found.");
                    }
                }
            }
            None => {
                panic!("Struct was not added to symbol table.");
            }
        }
    }

    #[test]
    fn parse_struct_definition_with_variable() {
        let mut scanner =
            Scanner::new_from_string(String::from("struct Person { int x; } person1;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        parser
            .parse_global_declaration()
            .expect("Failed to define struct");

        let struct_sym = match parser
            .composite_symbol_table
            .find_symbol("Person", SymClass::STRUCT)
        {
            Some(sym) => {
                let sym = Rc::clone(&sym);
                assert_eq!(sym.borrow().name, "Person");
                assert_eq!(sym.borrow().sym_type, SymType::STRUCT);
                assert_eq!(sym.borrow().sym_class, SymClass::STRUCT);
                match &sym.borrow().members {
                    Some(member) => {
                        let member = member.borrow();
                        assert_eq!(member.name, "x");
                        assert_eq!(member.sym_type, SymType::VARIABLE);
                        assert_eq!(member.sym_class, SymClass::MEMBER);
                    }
                    None => {
                        panic!("Struct member was not found.");
                    }
                }
                sym
            }
            None => {
                panic!("Struct was not added to symbol table.");
            }
        };

        match parser
            .global_symbol_table
            .find_symbol("person1", SymClass::GLOBAL)
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.name, "person1");
                assert_eq!(sym.sym_type, SymType::VARIABLE);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert!(Rc::ptr_eq(&struct_sym, sym.type_sym.as_ref().unwrap()));
            }
            None => {
                panic!("Struct variable was not found in the symbol table.");
            }
        }
    }

    #[test]
    fn parse_struct_variable_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("struct Person person1;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let struct_sym = parser.composite_symbol_table.add_symbol(
            "Person".to_string(),
            DataType::NONE,
            0,
            SymType::STRUCT,
            0,
            SymClass::STRUCT,
        );

        parser
            .parse_global_declaration()
            .expect("Failed to define struct");

        match parser
            .global_symbol_table
            .find_symbol("person1", SymClass::GLOBAL)
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.name, "person1");
                assert_eq!(sym.sym_type, SymType::VARIABLE);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert!(Rc::ptr_eq(&struct_sym, sym.type_sym.as_ref().unwrap()));
            }
            None => {
                panic!("Struct variable was not found in the symbol table.");
            }
        }
    }
}
