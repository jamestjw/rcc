use super::*;

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
                TokenType::SEMI => {
                    self.parse_global_var_declaration(ident)?;
                    Ok(None)
                }
                _ => Err(format!("Syntax error, unexpected '{}' found", tok.token_type).into()),
            },
            None => {
                panic!("Unexpected error: No more tokens found in parse_statement.");
            }
        }
    }

    fn parse_global_var_declaration(&mut self, ident: Token) -> Result<(), Box<dyn Error>> {
        self.match_token(TokenType::SEMI)?;

        // TODO: Allow assignation of initial value during declaration
        self.add_global_symbol(ident.lexeme, DataType::INT, 0, SymType::VARIABLE);

        Ok(())
    }

    fn parse_func_declaration(&mut self, ident: Token) -> Result<Box<ASTnode>, Box<dyn Error>> {
        // TODO: Support defining parameters
        self.match_token(TokenType::LPAREN)?;
        self.match_token(TokenType::RPAREN)?;

        self.match_token(TokenType::LBRACE)?;

        let mut tree: Option<Box<ASTnode>> = None;

        // TODO: Figure out the right return type
        // We add the symbol here so that recursive calls work
        let sym = self.add_global_symbol(ident.lexeme, DataType::INT, 0, SymType::FUNCTION);

        while self.current_token.as_ref().unwrap().token_type != TokenType::RBRACE {
            let node = self.parse_statement()?;

            match tree {
                Some(original_tree) => {
                    tree = Some(ASTnode::new_boxed(ASTop::GLUE, original_tree, node))
                }
                None => {
                    tree = Some(node);
                }
            }
        }

        self.match_token(TokenType::RBRACE)?;

        let mut fn_node = ASTnode::new_unary(
            ASTop::FUNCTION,
            tree.unwrap_or_else(|| ASTnode::new_noop()), // TODO: Make this return ASTop::RETURN by default
        );

        fn_node.symtable_entry = Some(sym);

        Ok(fn_node)
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
    fn parse_global_int_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int x;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap();

        assert!(expr.is_none());

        match parser.global_symbol_table.get("x") {
            Some(new_sym) => {
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::VARIABLE);
            }
            None => panic!("New symbol table entry not found"),
        }
    }

    #[test]
    fn parse_func_declaration() {
        let mut scanner = Scanner::new_from_string(String::from("int main() {}"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let expr = parser.parse_global_declaration().unwrap().unwrap();

        let expected = ASTnode::new_unary(ASTop::FUNCTION, ASTnode::new_noop());
        match_ast_node(Some(&expr), expected);
        match parser.global_symbol_table.get("main") {
            Some(new_sym) => {
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
                assert_eq!(new_sym.sym_type, SymType::FUNCTION);
            }
            None => panic!("New symbol table entry not found"),
        }
    }
}
