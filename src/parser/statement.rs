use super::*;

impl<'a> Parser<'a> {
    pub fn parse_statements(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let mut tree: Option<Box<ASTnode>> = None;

        while self.current_token.as_ref().unwrap().token_type != TokenType::EOF {
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
                TokenType::INT => self.parse_global_declaration()?,
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

    fn parse_global_declaration(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        self.match_token(TokenType::INT)?;
        let ident = self.match_token(TokenType::IDENT)?;
        self.match_token(TokenType::SEMI)?;

        // TODO: Allow assignation of initial value during declaration
        self.add_global_symbol(ident.lexeme, DataType::INT, 0);

        // TODO: Remove this when we handle global declarations separately
        Ok(ASTnode::new_noop())
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
        let expected = ASTnode::new_noop();

        match_ast_node(Some(&expr), expected);

        match parser.global_symbol_table.get("x") {
            Some(new_sym) => {
                assert_eq!(new_sym.data_type, DataType::INT);
                assert_eq!(new_sym.initial_value, 0);
            }
            None => panic!("New symbol table entry not found"),
        }
    }
}
