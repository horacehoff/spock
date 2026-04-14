![Spock logo](assets/spock_logo_horizontal.png)

# Spock

> [!WARNING]
> This language is experimental, and there may be logic-breaking bugs.
Expect breaking changes.

Spock is a WIP interpreted programming language written in Rust.
Its main goals are:

- Performance
- Ease-of-use and readabilty
- Syntax similarity with low-level languages

Its goal is to provide a faster alternative to Python, and one that's closer to
low-level languages, while still being accessible to a wide audience.

Key info:

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

## Documentation

The documentation is in the `docs/` folder.\
[Standard library documentation](docs/STD_LIB.md)

## Installation

No binaries are provided yet. You need to compile Spock yourself.

1. [Install Rust](https://rustup.rs/)
2. Clone the repo

```sh
git clone https://github.com/horacehoff/spock
```

3. Run/Build Spock

```diff
cargo run --release -- myfile.spock
cargo build --release
```

## Instruction Set

Spock uses an instruction set with a size of 8 bytes.
[Browse the instructions here](src/instr.rs).

```rs
let x = 20;
// parentheses are optional
if (x < 200) {
  print("TRUE!");
}
```

becomes...

```spock
0: SupEqIntJmp(0, 1, 2)   ─┐
1: Print(2)                X
```

---

```rs
let n = 40;
let a=0;
let b=1;
let c=0;
let i=0;
while i < n {
   c = a+b;
   a = b;
   b = c;
   i = i+1;
}
print(c);
```

becomes...

```spock
0: SupEqIntJmp(4, 0, 6)   ─┐ <─┐
1: AddInt(1, 2, 3)         │   │
2: Mov(2, 1)               │   │
3: Mov(3, 2)               │   │
4: AddInt(4, 5, 4)         │   │
5: JmpBack(5)              │  ─┘
6: Print(3)              <─┘
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

### Imports

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

## Types

- `Boolean` (`true`/`false`)
- `Integer` (i32)
- `Float` (f64)
- `Array` (`[1, 2, 3, "4", 5.0, true]`)
- `String`
- 