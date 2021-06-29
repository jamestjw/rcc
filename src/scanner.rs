// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree. 

use std::fmt;
use std::fs;

pub mod token;

use token::{Token, TokenType};

pub struct Scanner {
    input_file_chars: Box<dyn Iterator<Item = char>>,
    current_token: Token,
    line_number: u16,
    putback_char: Option<char>,
}

#[derive(Debug)]
pub struct ScannerError(String);

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Scanner {
    // Loads input file into the scanner, returns an error if it fails to read
    // the said file.
    // TODO: Currently the scanner loads the entire input file, perhaps
    // it is possible to incremental read parts of the file into a buffer instead
    // Idea: Load entire line so that we can even illustrate where errors are found
    pub fn new(input_fname: &str) -> Result<Scanner, ScannerError> {
        let file_contents = fs::read_to_string(input_fname)
            .map_err(|e| ScannerError(format!("Error reading file {}: {}", input_fname, e)))?;

        Ok(Scanner::new_from_string(file_contents))
    }

    fn new_from_string(input_str: String) -> Scanner {
        Scanner {
            input_file_chars: Box::new(input_str.chars().collect::<Vec<_>>().into_iter()),
            current_token: Token::new(TokenType::EOF),
            line_number: 1,
            putback_char: None,
        }
    }

    // Returns a pointer to current Token
    pub fn current(&self) -> &Token {
        &self.current_token
    }

    // Consume current token and scan in the next one
    pub fn consume(&mut self) -> Result<&Token, ScannerError> {
        let mut int_value = 0;
        let token_type = match self.next_char() {
            Some(c) => match c {
                '+' => TokenType::PLUS,
                '-' => TokenType::MINUS,
                '*' => TokenType::STAR,
                '/' => TokenType::SLASH,
                ';' => TokenType::SEMI,
                '(' => TokenType::LPAREN,
                ')' => TokenType::RPAREN,
                '0'..='9' => {
                    int_value = self.scan_int(c);
                    TokenType::INTLIT
                }
                c if c.is_alphabetic() => {
                    let lexeme = self.scan_ident(c);
                    match self.token_type_from_lexeme(&lexeme) {
                        Ok(t) => t,
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                _ => return Err(ScannerError(format!("Unknown character found: {}", c))),
            },
            None => TokenType::EOF,
        };

        self.current_token = Token {
            token_type,
            int_value,
        };

        Ok(&self.current_token)
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
    fn token_type_from_lexeme(&self, lexeme: &str) -> Result<TokenType, ScannerError> {
        let token_type = match lexeme {
            lexeme if lexeme.starts_with('p') => match lexeme {
                "print" => Some(TokenType::PRINT),
                _ => None,
            },
            _ => None,
        };

        token_type.ok_or(ScannerError(format!(
            "Unknown token found: {} on line number {}",
            lexeme, self.line_number
        )))
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

    #[test]
    fn scan_tokens_print_statement() {
        let mut scanner = Scanner::new_from_string(String::from(" print(42+79);  "));

        assert_eq!(scanner.next_char(), Some('p'));
        assert_eq!(scanner.scan_ident('p'), "print");

        let expected_types: Vec<(TokenType,u32)> = vec![
            (TokenType::PRINT, 0),
            (TokenType::LPAREN, 0),
            (TokenType::INTLIT, 42),
            (TokenType::PLUS, 0),
            (TokenType::INTLIT, 79),
            (TokenType::RPAREN, 0),
            (TokenType::SEMI, 0),
            (TokenType::EOF, 0),
        ];

        let mut res_idx = 1;
        loop {
            match scanner.consume() {
                Ok(tok) => {
                    let (token_type, int_value) = expected_types[res_idx];
                    assert_eq!(tok.token_type, token_type);
                    assert_eq!(tok.int_value, int_value);
                    res_idx += 1;
                    if tok.token_type == TokenType::EOF {
                        break;
                    }
                }
                Err(e) => {
                    panic!("Failed to consume token with error {}", e);
                }
            };
        }
    }
}
