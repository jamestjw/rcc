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
        STRLIT,
        IDENT,
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,
        CHAR,
        INT,
        VOID,
        STRUCT,
        ENUM,
        SEMI,
        RETURN,
        BANG,
        COMMA,
        AMPERSAND,
        LBRACKET,
        RBRACKET,
        DOT,
        COLON,
        ARROW,
        IF,
        ELSE,
        WHILE,
        FOR,
        SWITCH,
        CASE,
        DEFAULT,
        BREAK,
        CONTINUE,
        EQ,
        NOTEQ,
        GTEQ,
        GT,
        LTEQ,
        LT,
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
            TokenType::STRLIT => write!(f, "string literal"),
            TokenType::IDENT => write!(f, "identifier"),
            TokenType::LPAREN => write!(f, "("),
            TokenType::RPAREN => write!(f, ")"),
            TokenType::LBRACE => write!(f, "{{"),
            TokenType::RBRACE => write!(f, "}}"),
            TokenType::CHAR => write!(f, "char"),
            TokenType::INT => write!(f, "int"),
            TokenType::VOID => write!(f, "void"),
            TokenType::STRUCT => write!(f, "struct"),
            TokenType::ENUM => write!(f, "enum"),
            TokenType::SEMI => write!(f, ";"),
            TokenType::RETURN => write!(f, "return"),
            TokenType::BANG => write!(f, "!"),
            TokenType::COMMA => write!(f, ","),
            TokenType::AMPERSAND => write!(f, "&"),
            TokenType::LBRACKET => write!(f, "["),
            TokenType::RBRACKET => write!(f, "]"),
            TokenType::DOT => write!(f, "."),
            TokenType::COLON => write!(f, ":"),
            TokenType::ARROW => write!(f, "->"),
            TokenType::IF => write!(f, "if"),
            TokenType::ELSE => write!(f, "else"),
            TokenType::WHILE => write!(f, "while"),
            TokenType::FOR => write!(f, "for"),
            TokenType::SWITCH => write!(f, "switch"),
            TokenType::CASE => write!(f, "case"),
            TokenType::DEFAULT => write!(f, "default"),
            TokenType::BREAK => write!(f, "break"),
            TokenType::CONTINUE => write!(f, "continue"),
            TokenType::GT => write!(f, ">"),
            TokenType::GTEQ => write!(f, ">="),
            TokenType::LT => write!(f, "<"),
            TokenType::LTEQ => write!(f, "<="),
            TokenType::EQ => write!(f, "=="),
            TokenType::NOTEQ => write!(f, "!="),
        }
    }
}

impl TokenType {
    pub fn convertible_to_data_type(&self) -> bool {
        match self {
            TokenType::INT | TokenType::CHAR | TokenType::STRUCT | TokenType::VOID => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub int_value: i32,
    // TODO: Investigate the feasibility of simply creating references to the
    // source program using Rc Strings.
    pub lexeme: String,
    // If the token represents a STRLIT, it will contain an ID to a string table entry
    pub string_table_id: Option<usize>,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String) -> Token {
        Token {
            token_type,
            int_value: 0,
            lexeme: lexeme,
            string_table_id: None,
        }
    }
}
