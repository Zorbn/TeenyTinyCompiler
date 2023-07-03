use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type EnvironmentRef = Rc<RefCell<Environment>>;

pub fn new_environment_ref(enclosing: Option<EnvironmentRef>) -> EnvironmentRef {
    Rc::new(RefCell::new(Environment::new(enclosing)))
}

pub struct Environment {
    enclosing: Option<EnvironmentRef>,
    symbols: HashMap<String, usize>,
}

impl Environment {
    pub fn new(enclosing: Option<EnvironmentRef>) -> Self {
        Self {
            enclosing,
            symbols: HashMap::new(),
        }
    }

    pub fn has_symbol(&self, symbol: &str) -> bool {
        self.get_symbol_type_id(symbol).is_some()
    }

    pub fn get_symbol_type_id(&self, symbol: &str) -> Option<usize> {
        if let Some(value_type) = self.symbols.get(symbol) {
            return Some(*value_type);
        }

        return match &self.enclosing {
            Some(enclosing) => RefCell::borrow(enclosing).get_symbol_type_id(symbol),
            _ => None,
        };
    }

    pub fn add_symbol(&mut self, symbol: &str, type_id: usize) {
        self.symbols.insert(symbol.into(), type_id);
    }
}
