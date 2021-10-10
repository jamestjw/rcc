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

    pub fn parse_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let statement = match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::RETURN => self.parse_return()?,
                TokenType::IF => self.parse_if_statement()?,
                TokenType::WHILE => self.parse_while_statement()?,
                TokenType::FOR => self.parse_for_statement()?,
                TokenType::SWITCH => self.parse_switch_statement()?,
                TokenType::BREAK => self.parse_break_statement()?,
                TokenType::CONTINUE => self.parse_continue_statement()?,
                TokenType::LBRACE => {
                    self.match_token(TokenType::LBRACE)?;
                    let stmt = self.parse_compound_statement(false)?;
                    self.match_token(TokenType::RBRACE)?;
                    stmt
                }
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
            TokenType::ENUM => {
                self.parse_enum_definition()?;
                Ok(None)
            }
            _ => {
                // TODO: Handle composite types
                let (data_type, _) = self.parse_type()?;
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
                        panic!(
                            "Unexpected error: No more tokens found in parse_global_declaration."
                        );
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
        let (data_type, comp_symbol_entry) = self.parse_type()?;

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
        sym.type_sym = comp_symbol_entry;

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
    // TODO: Handle anonymous structs
    fn parse_struct_definition(&mut self) -> Result<(), Box<dyn Error>> {
        self.match_token(TokenType::STRUCT)?;
        let struct_name_tok = self.match_token(TokenType::IDENT)?;

        let next_token = self.current_token_type()?;

        match next_token {
            // Creating variable from previously defined struct
            // Could be a pointer to a struct
            // e.g. struct Person person1;
            TokenType::IDENT | TokenType::STAR => {
                self.parse_composite_variable_declaration(
                    &struct_name_tok.lexeme,
                    SymClass::STRUCT,
                    DataType::STRUCT,
                )?;
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
                    // Could be defining a pointer to a struct
                    TokenType::IDENT | TokenType::STAR => {
                        self.parse_composite_variable_declaration(
                            &struct_name_tok.lexeme,
                            SymClass::STRUCT,
                            DataType::STRUCT,
                        )?;
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

    fn parse_struct_members_declaration(&mut self) -> Result<SymbolTableEntry, Box<dyn Error>> {
        // TODO: Support nested structs
        // TODO: Support arrays in structs
        let (data_type, _) = self.parse_type()?;

        if data_type == DataType::VOID {
            return Err("Unable to define struct members with void type.".into());
        }

        let ident = self.match_token(TokenType::IDENT)?;

        // TODO: Verify uniqueness of members
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

    fn parse_enum_definition(&mut self) -> Result<(), Box<dyn Error>> {
        self.match_token(TokenType::ENUM)?;

        let enum_name_tok = self.match_token(TokenType::IDENT)?;

        let next_token = self.current_token_type()?;

        match next_token {
            // Creating variable from previously defined struct
            // Could be a pointer to a struct
            TokenType::IDENT | TokenType::STAR => {
                self.parse_composite_variable_declaration(
                    &enum_name_tok.lexeme,
                    SymClass::ENUM,
                    DataType::ENUM,
                )?;
                self.match_token(TokenType::SEMI)?;
                Ok(())
            }
            TokenType::LBRACE => {
                if self
                    .composite_symbol_table
                    .find_symbol(&enum_name_tok.lexeme, None)
                    .is_some()
                {
                    return Err(format!(
                        "Tried to define enum '{}' that already exists.",
                        &enum_name_tok.lexeme
                    )
                    .into());
                }

                let sym = self.composite_symbol_table.add_symbol(
                    enum_name_tok.lexeme.clone(),
                    DataType::NONE,
                    0,
                    SymType::ENUM,
                    0,
                    SymClass::ENUM,
                );

                self.match_token(TokenType::LBRACE)?;

                // Only parse members if the struct is not empty
                if self.is_token_type(TokenType::RBRACE)? {
                    return Err("Enums with no members are invalid.".into());
                }

                let mut next_index = 0;

                loop {
                    if self.is_token_type(TokenType::RBRACE)? {
                        break;
                    }

                    let enum_mem_tok = self.match_token(TokenType::IDENT)?;

                    if self.is_token_type(TokenType::ASSIGN)? {
                        self.consume()?;
                        let enum_int_tok = self.match_token(TokenType::INTLIT)?;
                        if enum_int_tok.int_value <= next_index {
                            return Err(
                                format!("Invalid enum value {}.", enum_int_tok.int_value).into()
                            );
                        }
                        next_index = enum_int_tok.int_value;
                    }

                    if self
                        .global_symbol_table
                        .find_symbol(&enum_mem_tok.lexeme, None)
                        .is_some()
                    {
                        return Err(format!(
                            "Tried to define enumerator '{}' that already exists.",
                            &enum_mem_tok.lexeme
                        )
                        .into());
                    }

                    let member_sym = self.global_symbol_table.add_symbol(
                        // TODO: Parse datatype
                        enum_mem_tok.lexeme.clone(),
                        DataType::INT,
                        next_index,
                        SymType::ENUMERATOR,
                        0,
                        // TODO: Should this be global?
                        SymClass::GLOBAL,
                    );
                    member_sym.borrow_mut().type_sym = Some(Rc::clone(&sym));

                    next_index += 1;

                    if self.is_token_type(TokenType::COMMA)? {
                        self.consume()?;
                    }
                }

                self.match_token(TokenType::RBRACE)?;

                // We might have a enum variable definition following definition of a enum.
                let next_token = self.current_token_type()?;

                match next_token {
                    TokenType::SEMI => {}
                    // Could be defining a pointer to a enum
                    TokenType::IDENT | TokenType::STAR => {
                        self.parse_composite_variable_declaration(
                            &enum_name_tok.lexeme,
                            SymClass::ENUM,
                            DataType::ENUM,
                        )?;
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

    fn parse_composite_variable_declaration(
        &mut self,
        comp_name: &str,
        composite_sym_class: SymClass,
        data_type: DataType,
    ) -> Result<(), Box<dyn Error>> {
        let indirection_count = self.parse_indirection()?;
        let mut final_data_type = data_type;
        for _ in 0..indirection_count {
            final_data_type = to_pointer(final_data_type)?;
        }

        let ident = self.match_token(TokenType::IDENT)?;
        let class_name_str = composite_sym_class.name();
        match self
            .composite_symbol_table
            .find_symbol(comp_name, Some(composite_sym_class))
        {
            Some(comp_sym) => {
                // We add the symbol here so that recursive calls work
                let sym = self.global_symbol_table.add_symbol(
                    ident.lexeme,
                    final_data_type,
                    0,
                    SymType::VARIABLE,
                    0,
                    SymClass::GLOBAL,
                );
                sym.borrow_mut().type_sym = Some(Rc::clone(&comp_sym));

                Ok(())
            }
            None => Err(format!("Referencing undefined {}: {}", class_name_str, comp_name).into()),
        }
    }

    fn parse_compound_statement(
        &mut self,
        return_check: bool,
    ) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let mut tree: Option<Box<ASTnode>> = None;
        // TODO: Is there a better way to track whether or not a return was included?
        let mut last_op = ASTop::NOOP;

        loop {
            let curr_token_type = self.current_token_type()?;
            if curr_token_type == TokenType::RBRACE {
                break;
            }
            // Allowing multiline statements in switch cases without braces
            if self.switch_count > 0
                && (curr_token_type == TokenType::CASE || curr_token_type == TokenType::DEFAULT)
            {
                break;
            }

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
        if return_check && last_op != ASTop::RETURN {
            return Err("Return value must be provided in non-void function.".into());
        }

        Ok(tree.unwrap_or_else(|| ASTnode::new_noop()))
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

        self.match_token(TokenType::LPAREN)?;

        if !self.is_token_type(TokenType::RPAREN)? {
            sym.borrow_mut()
                .add_member(self.parse_func_param_declaration()?);
        }

        self.match_token(TokenType::RPAREN)?;
        self.match_token(TokenType::LBRACE)?;

        // Point to the symbol of the function we are parsing so that it can be referenced
        // when we parse the body of the function.
        self.current_func_sym = Some(Rc::clone(&sym));

        let tree = self.parse_compound_statement(data_type != DataType::VOID)?;

        // Since we are done parsing the body of the function, we remove the pointer
        // to the symbol of the current function.
        self.current_func_sym = None;

        self.match_token(TokenType::RBRACE)?;

        let mut fn_node = ASTnode::new_unary(ASTop::FUNCTION, tree, DataType::NONE);

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

    pub fn parse_if_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        self.match_token(TokenType::IF)?;
        self.match_token(TokenType::LPAREN)?;
        let cond_expr = self.expr_by_precedence(0)?;
        self.match_token(TokenType::RPAREN)?;
        let if_body = self.parse_statement()?;

        let mut body_node = ASTnode::new_unary(ASTop::GLUE, if_body, DataType::NONE);

        if self.current_token_type()? == TokenType::ELSE {
            self.match_token(TokenType::ELSE)?;
            let else_body = self.parse_statement()?;
            body_node.right = Some(else_body);
        }

        Ok(ASTnode::new_boxed(
            ASTop::IF,
            cond_expr,
            body_node,
            DataType::NONE,
        ))
    }

    pub fn parse_while_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        self.loop_count += 1;
        self.match_token(TokenType::WHILE)?;
        self.match_token(TokenType::LPAREN)?;
        let cond_expr = self.expr_by_precedence(0)?;
        self.match_token(TokenType::RPAREN)?;
        let while_body = self.parse_statement()?;

        self.loop_count -= 1;

        Ok(ASTnode::new_boxed(
            ASTop::WHILE,
            cond_expr,
            while_body,
            DataType::NONE,
        ))
    }

    pub fn parse_break_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        if self.loop_count + self.switch_count <= 0 {
            return Err("Break statements are only valid in loops.".into());
        }

        self.match_token(TokenType::BREAK)?;
        self.match_token(TokenType::SEMI)?;

        Ok(ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE))
    }

    pub fn parse_continue_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        if self.loop_count <= 0 {
            return Err("Continue statements are only valid in loops.".into());
        }

        self.match_token(TokenType::CONTINUE)?;
        self.match_token(TokenType::SEMI)?;

        Ok(ASTnode::new_leaf(ASTop::CONTINUE, 0, DataType::NONE))
    }

    pub fn parse_for_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        // We cheat a little bit and we parse this into a WHILE statement
        // because all for loops can be treated as while loops
        self.loop_count += 1;

        self.match_token(TokenType::FOR)?;
        self.match_token(TokenType::LPAREN)?;
        let init_expr = self.parse_local_expression(TokenType::SEMI, TokenType::COMMA)?;
        self.match_token(TokenType::SEMI)?;
        let mut cond_expr = self.parse_local_expression(TokenType::SEMI, TokenType::COMMA)?;
        cond_expr = match cond_expr.op {
            ASTop::NOOP => ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
            _ => cond_expr,
        };
        self.match_token(TokenType::SEMI)?;
        let post_expr = self.parse_local_expression(TokenType::RPAREN, TokenType::COMMA)?;
        self.match_token(TokenType::RPAREN)?;

        self.match_token(TokenType::LBRACE)?;
        let body = self.parse_compound_statement(false)?;
        self.match_token(TokenType::RBRACE)?;

        self.loop_count -= 1;

        let node = ASTnode::new_boxed(
            ASTop::GLUE,
            init_expr,
            ASTnode::new_boxed(
                ASTop::FOR,
                cond_expr,
                ASTnode::new_boxed(ASTop::GLUE, body, post_expr, DataType::NONE),
                DataType::NONE,
            ),
            DataType::NONE,
        );

        Ok(node)
    }

    pub fn parse_switch_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        self.switch_count += 1;

        self.match_token(TokenType::SWITCH)?;
        self.match_token(TokenType::LPAREN)?;
        let expr = self.expr_by_precedence(0)?;
        self.match_token(TokenType::RPAREN)?;
        self.match_token(TokenType::LBRACE)?;

        let mut has_default = false;

        let mut case_nodes: Vec<Box<ASTnode>> = Vec::new();
        let mut case_values: Vec<i32> = Vec::new();

        loop {
            let curr_tok_type = self.current_token_type()?;
            match curr_tok_type {
                TokenType::CASE | TokenType::DEFAULT => {
                    let mut case_value = 0;
                    if has_default {
                        return Err(format!(
                            "No more cases can be defined after the default clause."
                        )
                        .into());
                    }
                    let op = match curr_tok_type {
                        TokenType::DEFAULT => {
                            has_default = true;
                            self.consume()?;
                            self.match_token(TokenType::COLON)?;
                            ASTop::DEFAULT
                        }
                        TokenType::CASE => {
                            self.consume()?;
                            let case_expr = self.primary_expr()?;
                            // TODO: Verify that there are no duplicates
                            if case_expr.op != ASTop::INTLIT {
                                return Err(
                                    "Only integer values are allowed as case values.".into()
                                );
                            } else {
                                case_value = case_expr.int_value;

                                if case_values.contains(&case_value) {
                                    return Err(format!(
                                        "Duplicate case value '{}' in switch statement.",
                                        case_value
                                    )
                                    .into());
                                }
                                case_values.push(case_value);
                            }
                            self.match_token(TokenType::COLON)?;
                            ASTop::CASE
                        }
                        _ => unreachable!("This arm should never be invoked."),
                    };
                    // Check if this case has a statement,
                    // i.e. whether or not it falls through
                    let case_stmt = match self.current_token_type()? {
                        TokenType::DEFAULT | TokenType::CASE => None,
                        _ => Some(self.parse_compound_statement(false)?),
                    };
                    let mut case_node = ASTnode::new_leaf(op, case_value, DataType::NONE);
                    case_node.left = case_stmt;
                    case_nodes.push(case_node);
                }
                TokenType::RBRACE => {
                    if case_nodes.len() == 0 {
                        return Err(format!("Encountered switch statement with 0 cases.").into());
                    }
                    break;
                }
                _ => {
                    return Err(format!(
                        "Unexpected {} token in switch statement.",
                        curr_tok_type.name()
                    )
                    .into());
                }
            }
        }
        self.match_token(TokenType::RBRACE)?;

        let mut switch_node = ASTnode::new_unary(ASTop::SWITCH, expr, DataType::NONE);

        for _ in 0..case_nodes.len() {
            let mut case_node = case_nodes.pop().unwrap();
            case_node.right = switch_node.right;
            switch_node.right = Some(case_node);
        }

        self.switch_count -= 1;
        Ok(switch_node)
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
            .find_symbol("x", Some(SymClass::GLOBAL))
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
            .find_symbol("x", Some(SymClass::GLOBAL))
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
            .find_symbol("y", Some(SymClass::GLOBAL))
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
            .find_symbol("x", Some(SymClass::GLOBAL))
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
            .find_symbol("x", Some(SymClass::GLOBAL))
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
            .find_symbol("y", Some(SymClass::GLOBAL))
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
            .find_symbol("main", Some(SymClass::GLOBAL))
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
            .find_symbol("test", Some(SymClass::GLOBAL))
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
            .find_symbol("test", Some(SymClass::GLOBAL))
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
    fn parse_func_declaration_with_struct_param() {
        let mut scanner =
            Scanner::new_from_string(String::from("int test(struct Person *p) { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let struct_sym = parser.composite_symbol_table.add_symbol(
            "Person".to_string(),
            DataType::NONE,
            0,
            SymType::STRUCT,
            0,
            SymClass::STRUCT,
        );

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
            .find_symbol("test", Some(SymClass::GLOBAL))
        {
            Some(new_sym) => {
                let new_sym = new_sym.borrow();
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::FUNCTION);
                assert_eq!(new_sym.sym_class, SymClass::GLOBAL);

                let param_sym = new_sym.members.as_ref().unwrap().borrow();
                assert_eq!(param_sym.data_type, DataType::STRUCTPTR);
                assert_eq!(param_sym.initial_value, 0);
                assert_eq!(param_sym.sym_type, SymType::VARIABLE);
                assert_eq!(param_sym.sym_class, SymClass::PARAM);
                assert_eq!(param_sym.name, "p");
                assert!(Rc::ptr_eq(
                    param_sym.type_sym.as_ref().unwrap(),
                    &struct_sym
                ));
            }
            None => panic!("New symbol table entry not found"),
        }
    }

    #[test]
    fn parse_func_declaration_with_invalid_struct_param() {
        let mut scanner =
            Scanner::new_from_string(String::from("int test(struct Person *p) { return 1; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        match parser.parse_global_declaration() {
            Ok(_) => {
                panic!("Parsing of function with invalid structt parameter should have failed.");
            }

            Err(e) => {
                assert_eq!(e.to_string(), "Referencing undefined struct Person.");
            }
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
            .find_symbol("Person", Some(SymClass::STRUCT))
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
            .find_symbol("Person", Some(SymClass::STRUCT))
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
            .find_symbol("person1", Some(SymClass::GLOBAL))
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
            .find_symbol("person1", Some(SymClass::GLOBAL))
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
    fn parse_if_statement_no_else() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("if(1) { x = 5; };"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let _sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let stmt = parser.parse_if_statement()?;

        let expected = ASTnode::new_boxed(
            ASTop::IF,
            ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
            ASTnode::new_unary(
                ASTop::GLUE,
                ASTnode::new_boxed(
                    ASTop::ASSIGN,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
                    DataType::INT,
                ),
                DataType::NONE,
            ),
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_if_statement_with_else() -> Result<(), Box<dyn Error>> {
        let mut scanner =
            Scanner::new_from_string(String::from("if(1) { x = 5; } else { x = 10; };"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let _sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let if_stmt = parser.parse_if_statement()?;

        let expected = ASTnode::new_boxed(
            ASTop::IF,
            ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
            ASTnode::new_boxed(
                ASTop::GLUE,
                ASTnode::new_boxed(
                    ASTop::ASSIGN,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
                    DataType::INT,
                ),
                ASTnode::new_boxed(
                    ASTop::ASSIGN,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 10, DataType::INT),
                    DataType::INT,
                ),
                DataType::NONE,
            ),
            DataType::NONE,
        );

        match_ast_node(Some(&if_stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_while_statement() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("while(x > 5) { x = x - 1; };"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let _sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );
        let stmt = parser.parse_while_statement()?;

        let expected = ASTnode::new_boxed(
            ASTop::WHILE,
            ASTnode::new_boxed(
                ASTop::GT,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
                DataType::INT,
            ),
            ASTnode::new_boxed(
                ASTop::ASSIGN,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                ASTnode::new_boxed(
                    ASTop::MINUS,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
                    DataType::INT,
                ),
                DataType::INT,
            ),
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_valid_break_statement_in_while_loop() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("while(1) { break; };"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_while_statement()?;

        let expected = ASTnode::new_boxed(
            ASTop::WHILE,
            ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
            ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_valid_continue_statement_in_while_loop() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("while(1) { continue; };"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_while_statement()?;

        let expected = ASTnode::new_boxed(
            ASTop::WHILE,
            ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
            ASTnode::new_leaf(ASTop::CONTINUE, 0, DataType::NONE),
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_invalid_break_statement_outside_of_loop() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("continue;"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_break_statement();

        match stmt {
            Ok(_) => {
                panic!("Should have failed to parse break statement outside of loop.");
            }
            Err(e) => assert_eq!("Break statements are only valid in loops.", e.to_string()),
        }

        Ok(())
    }

    #[test]
    fn parse_invalid_continue_statement_outside_of_loop() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from("continue;"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_continue_statement();

        match stmt {
            Ok(_) => {
                panic!("Should have failed to parse continue statement outside of loop.");
            }
            Err(e) => assert_eq!(
                "Continue statements are only valid in loops.",
                e.to_string()
            ),
        }

        Ok(())
    }

    #[test]
    fn parse_for_statement_with_all_exprs() -> Result<(), Box<dyn Error>> {
        let mut scanner =
            Scanner::new_from_string(String::from("for(x = 0; x < 5; x = x + 1) { y = x; }"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let _sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );

        let _sym_y = parser.global_symbol_table.add_symbol(
            "y".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );

        let stmt = parser.parse_for_statement()?;

        let init_expr = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_leaf(ASTop::INTLIT, 0, DataType::INT),
            DataType::INT,
        );
        let cond_expr = ASTnode::new_boxed(
            ASTop::LT,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            DataType::INT,
        );
        let post_expr = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_boxed(
                ASTop::ADD,
                ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                ASTnode::new_leaf(ASTop::INTLIT, 1, DataType::INT),
                DataType::INT,
            ),
            DataType::INT,
        );
        let body = ASTnode::new_boxed(
            ASTop::ASSIGN,
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
            DataType::INT,
        );
        let expected = ASTnode::new_boxed(
            ASTop::GLUE,
            init_expr,
            ASTnode::new_boxed(
                ASTop::FOR,
                cond_expr,
                ASTnode::new_boxed(ASTop::GLUE, body, post_expr, DataType::NONE),
                DataType::NONE,
            ),
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_switch_one_clause_simple() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) { 
                case 5:
                    break;
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_switch_statement()?;

        let mut case_1 = ASTnode::new_unary(
            ASTop::CASE,
            ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
            DataType::NONE,
        );
        case_1.int_value = 5;

        let expected = ASTnode::new_boxed(
            ASTop::SWITCH,
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            case_1,
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_switch_one_clause_with_two_stmts() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) { 
                case 5:
                    x = 10;
                    break;
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let _sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );

        let stmt = parser.parse_switch_statement()?;

        let mut case_1 = ASTnode::new_unary(
            ASTop::CASE,
            ASTnode::new_boxed(
                ASTop::GLUE,
                ASTnode::new_boxed(
                    ASTop::ASSIGN,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 10, DataType::INT),
                    DataType::INT,
                ),
                ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
                DataType::NONE,
            ),
            DataType::NONE,
        );
        case_1.int_value = 5;

        let expected = ASTnode::new_boxed(
            ASTop::SWITCH,
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            case_1,
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_switch_one_clause_with_two_stmts_with_braces() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) { 
                case 5: {
                    x = 10;
                    break;
                }
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let _sym_x = parser.global_symbol_table.add_symbol(
            "x".to_string(),
            DataType::INT,
            0,
            SymType::VARIABLE,
            0,
            SymClass::GLOBAL,
        );

        let stmt = parser.parse_switch_statement()?;

        let mut case_1 = ASTnode::new_unary(
            ASTop::CASE,
            ASTnode::new_boxed(
                ASTop::GLUE,
                ASTnode::new_boxed(
                    ASTop::ASSIGN,
                    ASTnode::new_leaf(ASTop::IDENT, 0, DataType::INT),
                    ASTnode::new_leaf(ASTop::INTLIT, 10, DataType::INT),
                    DataType::INT,
                ),
                ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
                DataType::NONE,
            ),
            DataType::NONE,
        );
        case_1.int_value = 5;

        let expected = ASTnode::new_boxed(
            ASTop::SWITCH,
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            case_1,
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_switch_two_clauses() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) { 
                case 5:
                    break;
                default:
                    break;
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_switch_statement()?;

        let case_2 = ASTnode::new_unary(
            ASTop::DEFAULT,
            ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
            DataType::NONE,
        );

        let mut case_1 = ASTnode::new_unary(
            ASTop::CASE,
            ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
            DataType::NONE,
        );
        case_1.int_value = 5;
        case_1.right = Some(case_2);

        let expected = ASTnode::new_boxed(
            ASTop::SWITCH,
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            case_1,
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_switch_two_clauses_with_fallthrough() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) { 
                case 5:
                default:
                    break;
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_switch_statement()?;

        let case_2 = ASTnode::new_unary(
            ASTop::DEFAULT,
            ASTnode::new_leaf(ASTop::BREAK, 0, DataType::NONE),
            DataType::NONE,
        );

        let mut case_1 = ASTnode::new_leaf(ASTop::CASE, 5, DataType::NONE);
        case_1.right = Some(case_2);

        let expected = ASTnode::new_boxed(
            ASTop::SWITCH,
            ASTnode::new_leaf(ASTop::INTLIT, 5, DataType::INT),
            case_1,
            DataType::NONE,
        );

        match_ast_node(Some(&stmt), expected);

        Ok(())
    }

    #[test]
    fn parse_switch_with_no_cases() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) { 
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_switch_statement();

        match stmt {
            Ok(_) => panic!("Expected failure to parse switch statement with no cases"),
            Err(e) => assert_eq!(e.to_string(), "Encountered switch statement with 0 cases."),
        }

        Ok(())
    }

    #[test]
    fn parse_switch_duplicate_cases() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) {
                case 2:
                case 2:
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_switch_statement();

        match stmt {
            Ok(_) => panic!("Expected failure to parse switch statement with duplicate cases"),
            Err(e) => assert_eq!(
                e.to_string(),
                "Duplicate case value '2' in switch statement."
            ),
        }

        Ok(())
    }

    #[test]
    fn parse_switch_with_case_after_default() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            switch(5) {
                case 2:
                default:
                case 4:
            } 
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        let stmt = parser.parse_switch_statement();

        match stmt {
            Ok(_) => panic!(
                "Expected failure to parse switch statement with cases after the default case."
            ),
            Err(e) => assert_eq!(
                e.to_string(),
                "No more cases can be defined after the default clause."
            ),
        }

        Ok(())
    }

    #[test]
    fn parse_enum_definition_without_variable() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            enum Person {
                a,
                b,
                c = 5,
                d,
            };
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        parser.parse_enum_definition()?;

        match parser
            .composite_symbol_table
            .find_symbol("Person", Some(SymClass::ENUM))
        {
            Some(sym) => {
                let sym = Rc::clone(&sym);
                assert_eq!(sym.borrow().name, "Person");
                assert_eq!(sym.borrow().sym_type, SymType::ENUM);
                assert_eq!(sym.borrow().sym_class, SymClass::ENUM);
                sym
            }
            None => {
                panic!("Enum was not added to symbol table.");
            }
        };

        match parser
            .global_symbol_table
            .find_symbol("a", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 0);
            }
            None => {
                panic!("Enumerator a was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("b", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 1);
            }
            None => {
                panic!("Enumerator b was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("c", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 5);
            }
            None => {
                panic!("Enumerator c was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("d", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 6);
            }
            None => {
                panic!("Enumerator d was not defined on the global symbol table.");
            }
        }

        Ok(())
    }

    #[test]
    fn parse_enum_definition_with_variable() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            enum Person {
                a,
                b,
                c = 5,
                d,
            } person;
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        parser.parse_enum_definition()?;

        match parser
            .composite_symbol_table
            .find_symbol("Person", Some(SymClass::ENUM))
        {
            Some(sym) => {
                let sym = Rc::clone(&sym);
                assert_eq!(sym.borrow().name, "Person");
                assert_eq!(sym.borrow().sym_type, SymType::ENUM);
                assert_eq!(sym.borrow().sym_class, SymClass::ENUM);
                sym
            }
            None => {
                panic!("Enum was not added to symbol table.");
            }
        };

        match parser
            .global_symbol_table
            .find_symbol("a", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 0);
            }
            None => {
                panic!("Enumerator a was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("b", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 1);
            }
            None => {
                panic!("Enumerator b was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("c", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 5);
            }
            None => {
                panic!("Enumerator c was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("d", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 6);
            }
            None => {
                panic!("Enumerator d was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("person", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::VARIABLE);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.data_type, DataType::ENUM);
            }
            None => {
                panic!("Enumerator d was not defined on the global symbol table.");
            }
        }

        Ok(())
    }

    #[test]
    fn parse_enum_definition_with_later_variable() -> Result<(), Box<dyn Error>> {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            enum Person {
                a,
                b,
                c = 5,
                d,
            };
            enum Person person;
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        parser.parse_enum_definition()?;
        parser.parse_enum_definition()?;

        match parser
            .composite_symbol_table
            .find_symbol("Person", Some(SymClass::ENUM))
        {
            Some(sym) => {
                let sym = Rc::clone(&sym);
                assert_eq!(sym.borrow().name, "Person");
                assert_eq!(sym.borrow().sym_type, SymType::ENUM);
                assert_eq!(sym.borrow().sym_class, SymClass::ENUM);
                sym
            }
            None => {
                panic!("Enum was not added to symbol table.");
            }
        };

        match parser
            .global_symbol_table
            .find_symbol("a", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 0);
            }
            None => {
                panic!("Enumerator a was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("b", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 1);
            }
            None => {
                panic!("Enumerator b was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("c", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 5);
            }
            None => {
                panic!("Enumerator c was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("d", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::ENUMERATOR);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.initial_value, 6);
            }
            None => {
                panic!("Enumerator d was not defined on the global symbol table.");
            }
        }

        match parser
            .global_symbol_table
            .find_symbol("person", Some(SymClass::GLOBAL))
        {
            Some(sym) => {
                let sym = sym.borrow();
                assert_eq!(sym.sym_type, SymType::VARIABLE);
                assert_eq!(sym.sym_class, SymClass::GLOBAL);
                assert_eq!(sym.data_type, DataType::ENUM);
            }
            None => {
                panic!("Enumerator d was not defined on the global symbol table.");
            }
        }

        Ok(())
    }

    #[test]
    fn parse_empty_enum_definition() {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            enum Person {};
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        match parser.parse_enum_definition() {
            Ok(_) => {}
            Err(e) => {
                assert_eq!(e.to_string(), "Enums with no members are invalid.");
            }
        }
    }

    #[test]
    fn parse_empty_enum_definition_with_invalid_value() {
        let mut scanner = Scanner::new_from_string(String::from(
            "
            enum Person {
                a,
                b,
                c = 1,
            };
            ",
        ));
        let mut parser = Parser::new(&mut scanner).unwrap();

        match parser.parse_enum_definition() {
            Ok(_) => {}
            Err(e) => {
                assert_eq!(e.to_string(), "Invalid enum value 1.");
            }
        }
    }
}
