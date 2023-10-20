
#' Generic function for inverse of an object.
#' @param x an object
#' @export
inv <- function(x, ...) {
    UseMethod("inv")
}

#' Default implementation for the inverse of a matrix.
#' @param x a matrix
#' @export
inv.matrix <- function(x, ...) {
    solve(x, ...)    
}

#' Default implementation for the inverse of a number.
#' @param x a number
#' @export
inv.numeric <- function(x, ...) {
    1/x
}

#' Generic function for the transpose of an object.
#' @param x an object
#' @export
transpose <- function(x, ...) {
    UseMethod("transpose")
}

#' Default implementation for the transpose of a matrix.
#' @param x a matrix
#' @export
transpose.matrix <- function(x, ...) {
    t(x)
}

#' Default implementation for the transpose of a number.
#' @param x a number
#' @export
transpose.numeric <- function(x, ...) {
    x
}

#' Generic function for the determinant of an object.
#' @param x an object
#' @export
det <- function(x, ...) {
    UseMethod("det")
}

#' Default implementation for the determinant of a matrix.
#' @param x a matrix
#' @export
det.matrix <- function(x, ...) {
    base::det(x, ...)
}

#' Default implementation for the determinant of a number.
#' @param x a number
#' @export
det.numeric <- function(x, ...) {
    x
}

#' Generic function for the trace of an object.
#' @param x an object
#' @export
trace <- function(x, ...) {
    UseMethod("trace")
}

#' Default implementation for the trace of a matrix.
#' @param x a matrix
#' @export
trace.matrix <- function(x, ...) {
    base::sum(diag(x))
}

#' Default implementation for the trace of a number.
#' @param x a number
#' @export
#' @note The trace of a number is the number itself.
#' @seealso \code{\link{diag}}
trace.numeric <- function(x, ...) {
    x
} 

#' Generic function for the diagonal of an object.
#' @param x an object
#' @export
diag <- function(x, ...) {
    UseMethod("diag")
}

#' Default implementation for the diagonal of a matrix.
#' @param x a matrix
#' @export
diag.matrix <- function(x, ...) {
    base::diag(x)
}

#' Default implementation for the diagonal of a number.
#' @param x a number
#' @export
diag.numeric <- function(x, ...) {
    x
}

#' Generic function for the rank of an object.
#' @param x an object
#' @export
rank <- function(x, ...) {
    UseMethod("rank")
}

#' Default implementation for the rank of a matrix.
#' @param x a matrix
#' @export
rank.matrix <- function(x, ...) {
    base::qr(x)$rank
}

#' A non-singular matrix has full rank.
#' 
#' @param x a non-singular (known to be) matrix
#' @export
rank.nonsingular_matrix <- function(x, ...) {
    nrow(x)
}

#' A singular matrix
#' @param x a singular (known to be) matrix
#' @export
inv.singular_matrix <- function(x, ...) {
    stop("Singular matrix")
}

#' Gradient of a function
#' @param f a function
#' @param ... additional arguments passed to `f`. you
#'            may, for instance, want to pass in a set
#'            of named parameters with which to take the
#'            gradient.
#' @export
gradient <- function(f, ...) {
    UseMethod("gradient")
}


#' Default implementation for the gradient of a function.
#' @param f a function
#' @param ... additional arguments (not used)
gradient.default <- function(f, ...) {
    0
}


#' Default implementation of gradient for an expression.
#' @param f an expression
#' @param ... additional arguments 
#' @export
gradient.expression <- function(f, ...) {
    for (i in seq_along(f)) {
        if (is.call(f[[i]])) {
            
        } else if (is.name(f[[i]])) {
            # TODO: check if the name is a parameter
        } else {
            print(f[[i]])
        }
    }
}

#' Default implementation of gradient for a function.
#' @param f a function
#' @param ... additional arguments
#' @export
gradient.function <- function(f, ...) {
    function(x) {
        numDeriv::grad(f, x, ...)
    }
}


#' Generic function for the derivative of an object.
#' @param f an expression
#' @param var the derivative is taken with respect to this variable
#' @param ... additional arguments 
#' @export
myderiv <- function(f, var) {
    UseMethod("myderiv")
}


#' Default implementation of the derivative of an expression.
#' @param f an expression
#' @param var the derivative is taken with respect to this variable
#' @param ... additional arguments 
#' @export
myderiv.expression <- function(f, var, ...) {
    print("inside expression")
    print(f)
    for (i in seq_along(f)) {
        if (is.call(f[[i]])) {
            return(myderiv(f[[i]], var))
        } else {
            return(0)
        }
    }
}

#' Default implementation of the derivative of a call expression.
#' @param f a call expression
#' @param var the derivative is taken with respect to this variable
#' @param ... additional arguments 
#' @export
myderiv.call <- function(f, var, ...) {
    print("inside call")
    print(f)
    for (i in seq_along(f)) {
        if (is.call(f[[i]])) {
            print(f[[i]])
            return(myderiv(f[[i]], var))
        } else if (is.name(f[[i]]) && f[[i]] == var) {
            return(1)
        } else if (is.name(f[[i]]) && f[[i]] == "+") {
            print("inside +")
            print(f[[i]])
            print(i)
            return(call(myderiv(f[[i-1]], var) + myderiv(f[[i+1]], var)))
        } else if (is.name(f[[i]]) && f[[i]] == "*") {
            print("inside *")
            print(f[[i]])
            print(i)
            return(call(myderiv(f[[i-1]], var) + myderiv(f[[i+1]], var)))
        } else {
            return(0)
        }
    }
}




#' walk the expression tree, not a generic method, a test case
#' @param e an expression
#' @param var find this one and highlight!
#' @export
walk <- function(e, var) {
    for (i in seq_along(e)) {
        if (is.call(e[[i]])) {
            cat("[", i, "] expression\n")
            print(e[[i]])
            walk(e[[i]], var)
        } else {
            if (e[[i]] == var) {
                cat("[", i, "] variable found\n")
            } else {
                cat("[", i, "] value\n")
            }
            cat("[", i, "] ", class(e[[i]]), " => ", e[[i]], "\n")
        }
    }
}



#' walk the expression tree, not a generic method, a test case
#' @param e an expression
#' @param var find this one and highlight!
#' @export
walk2 <- function(e, var) {
    for (x in e) {
        if (is.call(x)) {
            cat("[expression]\n")
            print(x)
            walk(x, var)
        } else {
            if (x == var) {
                cat("[variable]\n")
            } else {
                cat("[value]\n")
            }
            cat(class(x), " => ", x, "\n")
        }
    }
}
