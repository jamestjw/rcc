// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
pub struct StringTable {
    strings: Vec<String>,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            strings: Vec::new(),
        }
    }
    // This method might panic if called with an invalid ID.
    pub fn get_by_id(&self, id: usize) -> &str {
        &self.strings[id]
    }

    pub fn add(&mut self, s: String) -> usize {
        self.strings.push(s);
        self.strings.len() - 1
    }

    pub fn len(&self) -> usize {
        self.strings.len()
    }
}
