// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
use crate::enum_str;
use std::fmt;

enum_str! {
    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum TokenType {
        EOF,
        ASSIGN,
        PLUS,
        MINUS,
        SLASH,
        STAR,
        INTLIT,
        IDENT,
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,
        CHAR,
        INT,
        VOID,
        SEMI,
        RETURN,
        BANG,
        COMMA,
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::EOF => write!(f, "EOF"),
            TokenType::ASSIGN => write!(f, "="),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::SLASH => write!(f, "/"),
            TokenType::STAR => write!(f, "*"),
            TokenType::INTLIT => write!(f, "integer literal"),
            TokenType::IDENT => write!(f, "identifier"),
            TokenType::LPAREN => write!(f, "("),
            TokenType::RPAREN => write!(f, ")"),
            TokenType::LBRACE => write!(f, "{{"),
            TokenType::RBRACE => write!(f, "}}"),
            TokenType::CHAR => write!(f, "char"),
            TokenType::INT => write!(f, "int"),
            TokenType::VOID => write!(f, "void"),
            TokenType::SEMI => write!(f, ";"),
            TokenType::RETURN => write!(f, "return"),
            TokenType::BANG => write!(f, "!"),
            TokenType::COMMA => write!(f, ","),
        }
    }
}

pub struct Token {
    pub token_type: TokenType,
    pub int_value: i32,
    // TODO: Investigate the feasibility of simply creating references to the
    // source program using Rc Strings.
    pub lexeme: String,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String) -> Token {
        Token {
            token_type,
            int_value: 0,
            lexeme: lexeme,
        }
    }
}
