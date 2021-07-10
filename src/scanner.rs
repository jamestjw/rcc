// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use std::error::Error;
use std::fs;
use std::path::Path;

use crate::token::{Token, TokenType};

pub struct Scanner {
    input_file_chars: Box<dyn Iterator<Item = char>>,
    pub line_number: u16,
    putback_char: Option<char>,
}

impl Scanner {
    // Loads input file into the scanner, returns an error if it fails to read
    // the said file.
    // TODO: Currently the scanner loads the entire input file, perhaps
    // it is possible to incremental read parts of the file into a buffer instead
    // Idea: Load entire line so that we can even illustrate where errors are found
    pub fn new(input_fname: &Path) -> Result<Scanner, Box<dyn Error>> {
        let file_contents = fs::read_to_string(input_fname)
            .map_err(|e| format!("Error reading file {}: {}", input_fname.display(), e))?;

        Ok(Scanner::new_from_string(file_contents))
    }

    pub fn new_from_string(input_str: String) -> Scanner {
        Scanner {
            input_file_chars: Box::new(input_str.chars().collect::<Vec<_>>().into_iter()),
            line_number: 1,
            putback_char: None,
        }
    }

    // Consume current token and scan in the next one
    pub fn next_token(&mut self) -> Result<Token, Box<dyn Error>> {
        let mut int_value = 0;
        let mut lexeme = String::new();
        let token_type = match self.next_char() {
            Some(c) => match c {
                '=' => TokenType::ASSIGN,
                '+' => TokenType::PLUS,
                '-' => TokenType::MINUS,
                '*' => TokenType::STAR,
                '/' => TokenType::SLASH,
                ';' => TokenType::SEMI,
                '(' => TokenType::LPAREN,
                ')' => TokenType::RPAREN,
                '{' => TokenType::LBRACE,
                '}' => TokenType::RBRACE,
                ',' => TokenType::COMMA,
                '0'..='9' => {
                    int_value = self.scan_int(c) as i32;
                    TokenType::INTLIT
                }
                c if c.is_alphabetic() => {
                    lexeme.push_str(&self.scan_ident(c));
                    self.token_type_from_lexeme(&lexeme)
                }
                _ => return Err(format!("Unknown character found: {}", c).into()),
            },
            None => TokenType::EOF,
        };

        Ok(Token {
            token_type,
            int_value,
            lexeme,
        })
    }

    fn _next_char(&mut self) -> Option<char> {
        if self.putback_char.is_some() {
            return self.putback_char.take();
        }

        self.input_file_chars.next()
    }

    // Return next char in the input stream, skipping whitespace
    fn next_char(&mut self) -> Option<char> {
        // Keep looping until we find a char that is not whitespace,
        // or if we reach end of input stream.
        loop {
            match self._next_char() {
                Some(c) => {
                    if !c.is_ascii_whitespace() {
                        return Some(c);
                    }
                    if c == '\n' {
                        self.line_number += 1;
                    }
                }
                None => {
                    // Reached end of input stream
                    return None;
                }
            }
        }
    }

    fn putback_char(&mut self, c: char) {
        self.putback_char.replace(c);
    }

    // Scan an ident by passing in the first char that it starts with
    fn scan_ident(&mut self, c: char) -> String {
        let mut res = c.to_string();
        loop {
            // We want whitespace so we know when to stop
            match self._next_char() {
                Some(c) => match c {
                    c if c.is_alphanumeric() || c == '_' => {
                        res.push(c);
                    }
                    _ => {
                        self.putback_char(c);
                        break;
                    }
                },
                None => break,
            };
        }

        res
    }

    // To be called when current char is the first char
    // of an integer literal
    fn scan_int(&mut self, c: char) -> u32 {
        // TODO: Handle other radices
        let radix: u32 = 10;

        // TODO: Can I just unwrap here? If there is a
        // guarantee that the char contains a valid char
        // of this radix? Returning 0 by default now.
        let mut res = c.to_digit(radix).unwrap_or(0);

        loop {
            match self.next_char() {
                Some(c) => match c {
                    '0'..='9' => {
                        // Safe to unwrap as we already identified that chars
                        // are within the radix
                        res = res * radix + c.to_digit(radix).unwrap();
                    }
                    _ => {
                        self.putback_char(c);
                        break;
                    }
                },
                None => break,
            };
        }

        res
    }

    // Distinguish between identifiers and specific token types
    // based on a lexeme
    fn token_type_from_lexeme(&self, lexeme: &str) -> TokenType {
        if lexeme.starts_with('i') && lexeme == "int" {
            return TokenType::INT;
        }
        if lexeme.starts_with('r') && lexeme == "return" {
            return TokenType::RETURN;
        }
        if lexeme.starts_with('v') && lexeme == "void" {
            return TokenType::VOID;
        }

        return TokenType::IDENT;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn scan_one_ident() {
        let mut scanner = Scanner::new_from_string(String::from("print(5+2);"));

        assert_eq!(scanner.next_char(), Some('p'));
        assert_eq!(scanner.scan_ident('p'), "print");
    }

    #[test]
    fn scan_one_int_literal() {
        let mut scanner = Scanner::new_from_string(String::from("12345 +  2;"));

        assert_eq!(scanner.next_char(), Some('1'));
        assert_eq!(scanner.scan_int('1'), 12345);
    }
}
