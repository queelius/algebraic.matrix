algebraic.matrix
================
Alex Towell

  - [R Library: `algebraic.matrix`](#r-library-algebraicmatrix)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## R Library: `algebraic.matrix`

<!-- badges: start -->

<!-- badges: end -->

This is a algebraic matrix R library. It is mostly an API designed to
make working with matrices easier, where you can define your own matrix
type, say, a Hessian matrix, or a invertible matrix, and then use the
API to do operations on it. Often, since the matrix object is
specialized, the operations will be more efficient or more accurate than
the generic operations.
