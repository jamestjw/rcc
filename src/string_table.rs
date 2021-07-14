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
