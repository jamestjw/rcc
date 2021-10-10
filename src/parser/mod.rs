// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
use std::error::Error;

mod ast_node;
mod expression;
mod statement;
mod symbol_table;

use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
pub use ast_node::*;
use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

pub use symbol_table::*;

pub struct Parser<'a> {
    token_generator: &'a mut Scanner,
    current_token: Option<Token>,
    next_token: Option<Token>,
    pub global_symbol_table: SymbolTable,
    pub composite_symbol_table: SymbolTable,
    pub current_func_sym: Option<Rc<RefCell<SymbolTableEntry>>>, // Current function that we are parsing
    loop_count: u8,   // How many loops is the parser currently parsing
    switch_count: u8, // How many switches is the parser currently parsing
}

impl<'a> Parser<'a> {
    pub fn new(token_generator: &'a mut Scanner) -> Result<Parser<'a>, Box<dyn Error>> {
        let first_token = token_generator.next_token()?;
        Ok(Parser {
            token_generator,
            current_token: Some(first_token),
            next_token: None,
            global_symbol_table: SymbolTable::new(),
            composite_symbol_table: SymbolTable::new(),
            current_func_sym: None,
            loop_count: 0,
            switch_count: 0,
        })
    }

    // Consume current_token and load next token as the current_token
    fn consume(&mut self) -> Result<Token, Box<dyn Error>> {
        let next_token = if self.next_token.is_some() {
            self.next_token.take().unwrap()
        } else if self.current_token.is_none() {
            return Err("Unable to consume tokens when none are available.".into());
        } else {
            self.token_generator.next_token()?
        };

        let current_token_clone = self.current_token.replace(next_token);

        // Safe to unwrap since we know that it contains something
        Ok(current_token_clone.unwrap())
    }

    fn peek_next(&mut self) -> Result<Token, Box<dyn Error>> {
        // If next token is already loaded, just return it.
        if self.next_token.is_some() {
            return Ok(self.next_token.as_ref().cloned().unwrap());
        }

        // Next token is not yet loaded
        let next_token = self.token_generator.next_token()?;
        self.next_token.replace(next_token.clone());

        Ok(next_token)
    }

    // Consume the current if it matches the given type, else returns an error
    fn match_token(&mut self, token_type: TokenType) -> Result<Token, Box<dyn Error>> {
        match &self.current_token {
            Some(tok) => {
                if tok.token_type == token_type {
                    return self.consume();
                } else {
                    return Err(format!(
                        "Syntax error, expected '{}' token but '{}' was found instead.",
                        token_type, tok.token_type
                    )
                    .into());
                }
            }
            None => {
                return Err(format!(
                    "Syntax error, expected '{}' token but no more tokens were found.",
                    token_type
                )
                .into());
            }
        }
    }

    pub fn is_token_type(&self, token_type: TokenType) -> Result<bool, String> {
        match &self.current_token {
            Some(tok) => Ok(tok.token_type == token_type),
            None => Err("Unexpected end of input.".into()),
        }
    }

    pub fn current_token_type(&self) -> Result<TokenType, String> {
        match &self.current_token {
            Some(tok) => Ok(tok.token_type),
            None => Err("Unexpected end of input.".into()),
        }
    }

    // Parses the next token as a type, e.g. INT, VOID
    pub fn parse_type(
        &mut self,
    ) -> Result<(DataType, Option<Rc<RefCell<SymbolTableEntry>>>), Box<dyn Error>> {
        let mut comp_symbol_entry: Option<Rc<RefCell<SymbolTableEntry>>> = None;

        let mut dtype = match &self.current_token {
            Some(tok) => match tok.token_type {
                TokenType::CHAR | TokenType::INT | TokenType::VOID => {
                    let tok_type = tok.token_type;
                    self.consume()?;
                    DataType::try_from(tok_type)?
                }
                TokenType::STRUCT => {
                    self.consume()?;
                    let struct_name_tok = self.match_token(TokenType::IDENT)?;
                    comp_symbol_entry = match self
                        .composite_symbol_table
                        .find_symbol(&struct_name_tok.lexeme, Some(SymClass::STRUCT))
                    {
                        Some(sym) => Some(sym),
                        _ => {
                            return Err(format!(
                                "Referencing undefined struct {}.",
                                &struct_name_tok.lexeme
                            )
                            .into());
                        }
                    };
                    DataType::STRUCT
                }
                _ => {
                    return Err(format!(
                        "Expected data type but encountered {} instead.",
                        tok.token_type
                    )
                    .into());
                }
            },
            None => {
                return Err("Unexpected end of input.".into());
            }
        };

        let indirection_count = self.parse_indirection()?;

        for _ in 0..indirection_count {
            dtype = to_pointer(dtype)?;
        }

        Ok((dtype, comp_symbol_entry))
    }

    fn parse_indirection(&mut self) -> Result<u8, Box<dyn Error>> {
        let mut indirection_count = 0;

        loop {
            match &self.current_token {
                Some(tok) => match tok.token_type {
                    TokenType::STAR => {
                        indirection_count += 1;
                        self.consume()?;
                    }
                    _ => {
                        break;
                    }
                },
                None => return Err("Unexpected end of input.".into()),
            };
        }

        Ok(indirection_count)
    }
}

fn to_pointer(data_type: DataType) -> Result<DataType, String> {
    match data_type {
        DataType::INT => Ok(DataType::INTPTR),
        DataType::CHAR => Ok(DataType::CHARPTR),
        DataType::VOID => Ok(DataType::VOIDPTR),
        DataType::STRUCT => Ok(DataType::STRUCTPTR),
        _ => Err(format!(
            "Unable to convert {} type to a pointer.",
            data_type.name()
        )),
    }
}

pub fn pointer_to(data_type: DataType) -> Result<DataType, String> {
    match data_type {
        DataType::INTPTR => Ok(DataType::INT),
        DataType::CHARPTR => Ok(DataType::CHAR),
        DataType::STRUCTPTR => Ok(DataType::STRUCT),
        _ => Err(format!("{} is not a valid pointer type.", data_type.name())),
    }
}

#[cfg(test)]
fn match_ast_node(actual: Option<&Box<ASTnode>>, expected: Box<ASTnode>) {
    match actual {
        Some(actual) => {
            assert_eq!(actual.op, expected.op);
            assert_eq!(actual.int_value, expected.int_value);
            assert_eq!(actual.data_type, expected.data_type);
            if let Some(left) = expected.left {
                match_ast_node(actual.left.as_ref(), left);
            }
            if let Some(right) = expected.right {
                match_ast_node(actual.right.as_ref(), right);
            }
        }
        None => {
            panic!("Node empty.")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_type_with_valid_type() {
        let mut scanner = Scanner::new_from_string(String::from("int x;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let res = parser.parse_type();

        match res {
            Ok((dtype, comp_symbol_entry)) => {
                assert_eq!(dtype, DataType::INT);
                assert!(comp_symbol_entry.is_none());

                // Check that the INT token has been consumed
                assert_eq!(parser.current_token.unwrap().token_type, TokenType::IDENT);
            }
            Err(e) => {
                panic!("Failed to parse type with unexpected error: {}", e);
            }
        }
    }

    #[test]
    fn parse_type_with_invalid_type() {
        let mut scanner = Scanner::new_from_string(String::from("double x;"));
        let mut parser = Parser::new(&mut scanner).unwrap();
        let res = parser.parse_type();

        match res {
            Ok(_) => {
                panic!("Parsing of type should have failed");
            }
            Err(e) => {
                assert_eq!(
                    "Expected data type but encountered identifier instead.",
                    e.to_string()
                );
            }
        }
    }

    #[test]
    fn peeking() {
        let mut scanner = Scanner::new_from_string(String::from("int x = 5 + 3;"));
        let mut parser = Parser::new(&mut scanner).unwrap();

        assert_eq!(parser.current_token.clone().unwrap().lexeme, "int");
        assert_eq!(parser.peek_next().unwrap().clone().lexeme, "x");
        // Test that peeking twice doesn't change anything.
        assert_eq!(parser.peek_next().unwrap().clone().lexeme, "x");
        assert_eq!(parser.consume().unwrap().lexeme, "int");
        assert_eq!(parser.current_token.clone().unwrap().lexeme, "x");
        assert_eq!(
            parser.peek_next().unwrap().clone().token_type,
            TokenType::ASSIGN
        );
        assert_eq!(
            parser.peek_next().unwrap().clone().token_type,
            TokenType::ASSIGN
        );
    }
}
