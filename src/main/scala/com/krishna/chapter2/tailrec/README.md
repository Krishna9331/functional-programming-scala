## Tail Recursion

A call is said to be in 'tail position' if the caller does nothing other
than return the value of the recursive call.

The advantage we get having tail recursion is, it happens in only one
call stack frame. Since all the calculation happens before the method call
hence while making the recursive call it already has all the local variable
in function and it does not have to maintain it in stack. Hence same stack frame is used.
This saves us from Stack limit exception.


# Function
functions and methods are not exactly the same
thing in Scala. When we define a function literal, what is actually being
defined is an object with a method called . apply Scala has a special
rule for this method name, so that objects that have an apply method
can be called as if they were themselves methods.

(a, b) => a < b means

val lessThan = new Function2[Int, Int, Boolean] {
def apply(a: Int, b: Int) = a < b
}