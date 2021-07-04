// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
use std::collections::HashMap;
use std::error::Error;

mod ast_node;
mod expression;
mod statement;
mod symbol_table;

use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
pub use ast_node::*;
use std::rc::Rc;
pub use symbol_table::*;

pub struct Parser<'a> {
    token_generator: &'a mut Scanner,
    current_token: Option<Token>,
    pub global_symbol_table: HashMap<String, Rc<SymbolTableEntry>>,
}

impl<'a> Parser<'a> {
    pub fn new(token_generator: &'a mut Scanner) -> Result<Parser<'a>, Box<dyn Error>> {
        let first_token = token_generator.next_token()?;
        Ok(Parser {
            token_generator,
            current_token: Some(first_token),
            global_symbol_table: HashMap::new(),
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

    pub fn add_global_symbol(
        &mut self,
        lexeme: String,
        data_type: DataType,
        initial_value: i32,
    ) -> Rc<SymbolTableEntry> {
        let sym = Rc::new(SymbolTableEntry {
            data_type,
            initial_value,
            name: lexeme.clone(),
            size: 4,
        });
        self.global_symbol_table.insert(lexeme, Rc::clone(&sym));
        sym
    }
}

#[cfg(test)]
fn match_ast_node(actual: Option<&Box<ASTnode>>, expected: Box<ASTnode>) {
    match actual {
        Some(actual) => {
            assert_eq!(actual.op, expected.op);
            assert_eq!(actual.int_value, expected.int_value);
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
