#[derive(Debug, PartialEq)]
pub enum DataType {
    INT,
}

#[derive(Debug, PartialEq)]
pub enum SymType {
    VARIABLE,
    FUNCTION,
}

// TODO: Is there any way to reduce the number of possibly useless fields?
// i.e. initial_value is irrelevant if sym_type == FUNCTION
pub struct SymbolTableEntry {
    pub data_type: DataType,
    pub initial_value: i32, // Initial value for ints
    pub name: String,
    pub size: u8, // Size of symbol, i.e. sizeof(name)
    pub sym_type: SymType,
}
