// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
use std::error::Error;

use crate::enum_str;
use crate::scanner::{
    token::{Token, TokenType},
    Scanner,
};

enum_str! {
    #[derive(PartialEq)]
    pub enum ASTop {
        ADD,
        MINUS,
        MULTIPLY,
        DIVIDE,
        INTLIT,
        PRINT,
    }

}

pub struct ASTnode {
    pub op: ASTop,
    pub left: Option<Box<ASTnode>>,
    pub right: Option<Box<ASTnode>>,
    pub int_value: i32,
}

pub struct Parser<'a> {
    token_generator: &'a mut Scanner,
    current_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(token_generator: &'a mut Scanner) -> Result<Parser<'a>, Box<dyn Error>> {
        let first_token = token_generator.next_token()?;
        Ok(Parser {
            token_generator,
            current_token: Some(first_token),
        })
    }

    // TODO: Split up statements and expressions to separate files
    pub fn print_statement(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        self.match_token(TokenType::PRINT)?;
        self.match_token(TokenType::LPAREN)?;
        let binary_node = self.binary_expr()?;
        self.match_token(TokenType::RPAREN)?;
        self.match_token(TokenType::SEMI)?;

        Ok(Box::new(ASTnode {
            op: ASTop::PRINT,
            left: Some(binary_node),
            right: None,
            int_value: 0,
        }))
    }

    pub fn binary_expr(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let left = self.primary_expr()?;

        // TODO: Support other operators and support nested expressions
        self.match_token(TokenType::PLUS)?;

        let right = self.primary_expr()?;

        Ok(Box::new(ASTnode {
            op: ASTop::ADD,
            left: Some(left),
            right: Some(right),
            int_value: 0,
        }))
    }

    pub fn primary_expr(&mut self) -> Result<Box<ASTnode>, Box<dyn Error>> {
        let int_token = self.match_token(TokenType::INTLIT)?;

        Ok(Box::new(ASTnode {
            op: ASTop::INTLIT,
            left: None,
            right: None,
            int_value: int_token.int_value,
        }))
    }

    // Consume current_token and load next token as the current_token
    fn consume(&mut self) -> Result<Token, Box<dyn Error>> {
        if self.current_token.is_none() {
            return Err("Unable to consume tokens when none are available.".into());
        }
        let next_token = self.token_generator.next_token()?;
        let current_token_clone = self.current_token.replace(next_token);

        // Safe to unwrap since we know that it contains something
        Ok(current_token_clone.unwrap())
    }

    fn match_token(&mut self, token_type: TokenType) -> Result<Token, Box<dyn Error>> {
        // TODO: Implement display trait for TokenType
        match &self.current_token {
            Some(tok) => {
                if tok.token_type == token_type {
                    return self.consume();
                } else {
                    return Err(format!(
                        "Syntax error, expected {} token but {} was found instead.",
                        token_type.name(),
                        tok.token_type.name()
                    )
                    .into());
                }
            }
            None => {
                return Err(format!(
                    "Syntax error, expected {} token but no more tokens were found.",
                    token_type.name()
                )
                .into());
            }
        }
    }
}
