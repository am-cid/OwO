
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
