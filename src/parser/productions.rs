
pub trait Node {
    fn token(&self) -> String;
}
pub trait Statement {
    fn token(&self) -> String;
    fn statement(&self);
}
pub trait Expression {
    fn token(&self) -> String;
    fn expression(&self);
}
// helper macros so we don't have to repeat the same code
// since token() should be the same for all 3 traits above
macro_rules! impl_statement {
    ($T:ident { fn token(&self) -> String { $($body:tt)* } }) => {
        impl Node for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
        }
        impl Statement for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
            fn statement(&self) {}
        }
    };
}
macro_rules! impl_expression {
    ($T:ident { fn token(&self) -> String { $($body:tt)* } }) => {
        impl Node for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
        }
        impl Expression for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
            fn expression(&self) {}
        }
    };
}
