# Haskell Expression Evaluator

This project implements a Haskell module, which serves as an evaluator for λ-expressions. The evaluator reduces a given λ-expression into its normal form following the normal order evaluation strategy. The purpose of this assignment is to strengthen understanding of λ-calculus and Haskell's data types and pattern matching.

Hw3.hs: Main Haskell module containing the implementation of the λ-expression evaluator.

### Module Structure

#### module Hw3 where: 
Defines the Haskell module name.

#### type Symbol = String: 
Alias for String type to represent variable names visually.
#### data Expr: 
Defines the data type representing λ-expressions with three constructors: Var, App, and Lambda.
#### instance Show Expr where: 
Implements the Show type class for pretty-printing λ-expressions.
#### eval :: Expr -> Expr: 
Implements the evaluation function to reduce λ-expressions to normal form following the normal order evaluation strategy.
