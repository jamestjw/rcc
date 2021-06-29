// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree. 

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    EOF,
    PLUS,
    MINUS,
    SLASH,
    STAR,
    SEMI,
    INTLIT,
    LPAREN,
    RPAREN,
    IDENT,
    PRINT, // Temporary until we have function calls
}


#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub int_value: u32,
}

impl Token {
    pub fn new(token_type: TokenType) -> Token {
        Token {
            token_type,
            int_value: 0,
        }
    }
}
