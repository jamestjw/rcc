use super::*;

impl<'a> Parser<'a> {
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
            ASTnode::new_boxed(ASTop::ADD, ASTnode::new_leaf(51), ASTnode::new_leaf(24)),
        );

        match_ast_node(Some(expr), expected);
    }
}
