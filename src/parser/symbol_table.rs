#[derive(Debug, PartialEq)]
pub enum DataType {
    INT,
}

pub struct SymbolTableEntry {
    pub data_type: DataType,
    pub initial_value: i32, // Initial value for ints
    pub name: String,
    pub size: u8, // Size of symbol, i.e. sizeof(name)
}
