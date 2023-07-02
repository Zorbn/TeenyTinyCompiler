use std::{collections::HashMap, cell::RefCell, rc::Rc};

use crate::parser::ValueType;

pub type EnvironmentRef = Rc<RefCell<Environment>>;

pub fn new_environment_ref(enclosing: Option<EnvironmentRef>) -> EnvironmentRef {
    Rc::new(RefCell::new(Environment::new(enclosing)))
}

pub struct Environment {
    enclosing: Option<EnvironmentRef>,
    symbols: HashMap<String, ValueType>,
}

impl Environment {
    pub fn new(enclosing: Option<EnvironmentRef>) -> Self {
        Self {
            enclosing,
            symbols: HashMap::new(),
        }
    }

    pub fn has_symbol(&self, symbol: &str) -> bool {
        self.get_symbol_type(symbol).is_some()
    }

    pub fn get_symbol_type(&self, symbol: &str) -> Option<ValueType> {
        if let Some(value_type) = self.symbols.get(symbol) {
            return Some(*value_type);
        }

        return match &self.enclosing {
            Some(enclosing) => RefCell::borrow(enclosing).get_symbol_type(symbol),
            _ => None,
        }
    }

    pub fn add_symbol(&mut self, symbol: &str, value_type: ValueType) {
        self.symbols.insert(symbol.into(), value_type);
    }
}