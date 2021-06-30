// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
use std::error::Error;

use crate::enum_str;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};

mod expression;
mod statement;

enum_str! {
    #[derive(Debug, PartialEq)]
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

impl ASTnode {
    fn new_leaf(int_value: i32) -> Box<ASTnode> {
        Box::new(ASTnode {
            op: ASTop::INTLIT,
            left: None,
            right: None,
            int_value: int_value,
        })
    }

    fn new_unary(op: ASTop, left: Box<ASTnode>) -> Box<ASTnode> {
        Box::new(ASTnode {
            op: op,
            left: Some(left),
            right: None,
            int_value: 0,
        })
    }

    fn new_boxed(op: ASTop, left: Box<ASTnode>, right: Box<ASTnode>) -> Box<ASTnode> {
        Box::new(ASTnode {
            op,
            left: Some(left),
            right: Some(right),
            int_value: 0,
        })
    }
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
}

#[cfg(test)]
fn match_ast_node(actual: Option<Box<ASTnode>>, expected: Box<ASTnode>) {
    match actual {
        Some(actual) => {
            assert_eq!(actual.op, expected.op);
            assert_eq!(actual.int_value, expected.int_value);
            if let Some(left) = expected.left {
                match_ast_node(actual.left, left);
            }
            if let Some(right) = expected.right {
                match_ast_node(actual.right, right);
            }
        }
        None => {
            panic!("Node empty.")
        }
    }
}
