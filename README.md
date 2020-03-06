# Koak

## Installation

clone the repository:

    git clone https://github.com/iElden/Koak
compile the project:

    make
run it with a file:

    ./koak my_file

<br>

## Language feature

#### Declare a variable
    a: double = 9.5
    b: double = a + 195.3
    
#### Declare a function
You can declare a function by using this syntax `def FUNC_NAME(VAR_NAME: VAR_TYPE, ...) : RETURN_TYPE`
    
    def square(x: int): int {
        x * x
    }
    
#### Cast a value
You can cast a value by using the syntax `cast<NEW_TYPE>(VALUE)`:
    
    def divide_by_two(x_int: int): double {
        x_double : double = cast<double>(x)
        x / 2
    }

#### Operator
Addition: `a + b`

Substraction: `a - b`

Multiplication: `a * b`

Division : `a / b`

Modulo : `a % b`

Power : `a ** b`

<br>

Bitwise OR : `a | b`

Bitwise AND : `a & b`

Bitwise XOR : `a ^ b`

Bitwise NOT : `~a`

Bitwise left shift : `a << b`

Bitwise right shift : `a >> b`

<br>

Equal : `a == b`

Not equal : `a != b`

Greater : `a > b`

Greater or equal : `a >= b`

Lesser : `a < b`

Lesser or equal : `a <= b`

Not : `!a`

<br>

Unary + : `+a`

Unary - : `-a`


