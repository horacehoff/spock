![Spock logo](assets/spock_logo_horizontal.png)

# Spock

> [!WARNING]
> Spock is experimental. Expect bugs and breaking changes.

Spock is a fast, statically-typed interpreted language that aims to combine Rust-like syntax with Python's ease-of-use.

Its goal is to provide a faster alternative to Python that sits closer to low-level languages while remaining accessible to a wide audience.

## Key Info

- ~2-10x faster than Python in most cases.
  [Comparisons](COMPARISONS.md)
- Inlined functions
- Constant folding
- Constant propagation
- Peephole optimization
- Basic optimization of conditions
- Basic loop summation optimization (will get better)
- Type inference, static type checking, supports polymorphism
- Monomorphization

## Installation

No binaries are provided yet. You need to compile Spock yourself.

1. [Install Rust](https://rustup.rs/)
2. Clone the repo

```sh
# 1. Install Rust: https://rustup.rs/
# 2. Clone
git clone https://github.com/horacehoff/spock
cd spock
# 3. Build
cargo build --release
# 4. Run
./target/release/spock myfile.spock
```

## Syntax

### Blocks

```rs
print("Beginning of program");
let y = 20;
// All blocks are anonymous namespace scopes
// (e.g. trying to access x outside of the following block would yield an error)
{
    let x = 10 + y;
    print(x);
}
```

### Conditions

```rs
let x = 20;
if x == 20 {
  print("20!");
} else if x == 15 {
  print("15!");
} else {
  print("else!");
}
```

### Inline conditions

```rs
let y = 10;
print(if y == 10 {"y is: 10"} else {"y is "+str(y)});
let x = if y == 42 {5} else {10};
```

### While loops

```rs
let i = 0;
while i < 10 {
  print(i);
  i += 1;
}
```

### For loops

```rs
// NOTE: using _ as the variable name in a for loop will discard the value, making the program faster, but preventing access to the element
for x in [0,1,2,3] {
  for y in "abcd" {
    print(x);
    print(y);
  }
}
```

### Loops

> Loops indefinitely until flow is stopped

```rs
let i = 0;
loop {
    i += 1;
    print("i is: "+str(i));
    if i == 10 {
        break;
    }
}
print("End of the loop!");
```

### Integer loops

> Loops over a range of integers

```rs
let x = 0;
// Loops from i=0 to i = max-1
for i in 0..10000000 {
    x += i;
}
print(x);
}
```

```rs
let x = 0;
// Defaults to 0
for i in ..10 {
    print(i);
    x += 1;
}
print(x);
```

### Match statements

> Match statements currently don't support binding variables

```rs
let x = "hello";
match x {
  "hello" => {
    print("Hi!");
  }
  "goodbye" => {
    print("Bye!");
  }
  _ => {
    print("You said: "+x);
  }
}
```

### Loop flow control

```rs
let i = 0;
while i < 10 {
  print(i);
  i += 1;
  if i > 5 {
    // exits the loop
    break;
  }
}
```

```rs
for x in [0,1,2,3] {
  for y in "abcd" {
    if x == 2 {
      // skip to the next loop iteration
      continue;
    }
    print(x);
    print(y);
  }
}
```

### Importing other `.spock` files

You can import other `.spock` files with the following syntax:
```spock
use "fibonacci.spock"

fn main() {print(fibonacci(25));}
```

All top-level functions from the imported file become available immediately.
Imports can be nested, and circular imports trigger an error and crash the program.

### Importing dynamic libraries

You can load functions from dynamic libraries by specifying each function's
signature, with the following syntax:

```spock
import lib.extension {
  function_return_type function_name(function_arg_type_1, function_arg_type_1, ..., function_arg_type_n);
}
```

For example:

```spock
import test.dylib {
    int add(int, int);
}

function fib(n) {
    if n <= 1 {
        return n;
    }
    return fib(test::add(n, -1)) + fib(test::add(n,-2));
}

function main() {
print(test::add(6, 1));
print(fib(25));
}
```

### Arrays

Arrays can only hold one type

```rs
let x = [0,1,2,3,4];
print(x[0]);
x[1] = "42";
for w in x {
  print(w);
}
```

### Arithmetic Operations

```rs
let x = 0;

x = x + 1;
x += 1;

x = x - 1;
x -= 1;

x = x * 1;
x *= 1;

x = x / 1;
x /= 1;

x = x % 1;
x %= 1;

x = x ^ 1;
x ^= 1;

print(x == 1);
print(x != 1);
print(x > 1);
print(x >= 1);
print(x < 1);
print(x <= 1);
print(x > 1 || x < 1);
print(x > 1 && x < 1);
```

### Built-in types

- `Boolean`
- `Integer` (i32)
- `Float` (f64)
- `Array<T>`
- `String`

## Documentation

- [Standard library](docs/STD_LIB.md)
- [File system library](docs/FS_LIB.md)
