// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use crate::string_table::StringTable;
use std::error::Error;
use std::fs;
use std::path::Path;

use crate::token::{Token, TokenType};

pub struct Scanner {
    input_file_chars: Box<dyn Iterator<Item = char>>,
    pub line_number: u16,
    putback_char: Option<char>,
    pub string_table: StringTable,
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
            string_table: StringTable::new(),
        }
    }

    // Consume current token and scan in the next one
    pub fn next_token(&mut self) -> Result<Token, Box<dyn Error>> {
        let mut int_value = 0;
        let mut lexeme = String::new();
        let mut string_table_id = None;
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
                '&' => TokenType::AMPERSAND,
                '[' => TokenType::LBRACKET,
                ']' => TokenType::RBRACKET,
                '.' => TokenType::DOT,
                '0'..='9' => {
                    int_value = self.scan_int(c) as i32;
                    TokenType::INTLIT
                }
                '\'' => {
                    int_value = self.scan_char()? as i32;
                    TokenType::INTLIT
                }
                '"' => {
                    string_table_id = Some(self.scan_strlit()?);
                    TokenType::STRLIT
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
            string_table_id,
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

    // Scans a char literal
    // To be called the current character is the quotation mark
    // that denotes the start of the char literal.
    fn scan_char(&mut self) -> Result<u8, Box<dyn Error>> {
        let c = match self._next_char() {
            Some(c) => {
                if c != '\\' {
                    c as u8
                } else {
                    let res = match self._next_char() {
                        Some(c) => {
                            match c {
                                // TODO: Handle other characters
                                '0' => '\0',
                                'n' => '\n',
                                _ => {
                                    return Err(format!("Unknown token '{}' found after '\\' while parsing char literal.", c).into());
                                }
                            }
                        }
                        None => {
                            return Err(
                                "Unexpected end of input stream while parsing char literal".into(),
                            );
                        }
                    };
                    res as u8
                }
            }
            None => {
                return Err("Unexpected end of input stream while parsing char literal".into());
            }
        };

        match self.next_char() {
            Some(c) => {
                if c != '\'' {
                    return Err("Expected quote to terminate char literal.".into());
                }
            }
            None => {
                return Err("Unexpected end of input stream while parsing char literal".into());
            }
        };
        Ok(c)
    }

    fn scan_strlit(&mut self) -> Result<usize, String> {
        let mut strlit = String::new();

        // TODO: Should we limit the length of string literals?
        loop {
            match self._next_char() {
                Some(c) => {
                    if c == '"' {
                        break;
                    }
                    strlit.push(c);
                }
                None => {
                    return Err(
                        "Unexpected end of input stream while parsing string literal".into(),
                    );
                }
            };
        }

        Ok(self.string_table.add(strlit))
    }

    // Distinguish between identifiers and specific token types
    // based on a lexeme
    fn token_type_from_lexeme(&self, lexeme: &str) -> TokenType {
        if lexeme.starts_with('c') && lexeme == "char" {
            return TokenType::CHAR;
        }
        if lexeme.starts_with('i') && lexeme == "int" {
            return TokenType::INT;
        }
        if lexeme.starts_with('r') && lexeme == "return" {
            return TokenType::RETURN;
        }
        if lexeme.starts_with('s') && lexeme == "struct" {
            return TokenType::STRUCT;
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

    #[test]
    fn scan_one_char() {
        let mut scanner = Scanner::new_from_string(String::from("'c';"));

        assert_eq!(scanner.next_char(), Some('\''));
        assert_eq!(scanner.scan_char().unwrap(), 'c' as u8);
        assert_eq!(scanner.next_char(), Some(';'));
    }

    #[test]
    fn scan_one_strlit() {
        let mut scanner = Scanner::new_from_string(String::from("\"Hello world!\";"));

        assert_eq!(scanner.next_char(), Some('"'));
        let string_table_id = scanner.scan_strlit().unwrap();
        assert_eq!(
            scanner.string_table.get_by_id(string_table_id),
            "Hello world!".to_string()
        );

        assert_eq!(scanner.next_char(), Some(';'));
    }
}
