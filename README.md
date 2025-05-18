Spock
===

> [!WARNING]
>
> This language is very experimental, and there may be logic-breaking bugs. Expect breaking changes.

A work-in-progress programming language written in Rust for the best performance possible, whose syntax takes inspiration from Rust and Python. Basically, its goal is to provide a faster alternative to Python, and one that's closer to low-level languages, while still being accessible to a wide audience.

Key info:

- ~5-10x faster than Python in most cases.
  Comparisons [here](COMPARISONS.md)
- Inlined functions
- Constant folding
- Constant propagation
- Peephole optimization
- Basic optimization of conditions
- Basic loop summation optimization (will get better)

## Installation
No binaries are provided yet. You need to compile Spock yourself.
1. [Install Rust](https://rustup.rs/)
2. Clone the repo
```sh
git clone https://github.com/horacehoff/spock
```
3. Run/Build Spock
```diff
+ Using --release is very recommended, as it's much faster and doesn't print Spock debug information
cargo run --release
cargo build --release
```

## Numeric type
By default, Spock uses `f64` as its numeric type. If desired, you can run/build Spock with `i64` as the numeric type instead of `f64` by enabling the "int" feature:
```sh
cargo run --release --features "int"
cargo build --release --features "int"
```
This is useful in cases where `i64` can be faster than `f64`. Please note that, unless you know what you're doing, you should use the default numeric type.

## Instruction Set

Spock uses an instruction set with a size of 8 bytes.
The available instructions can be seen [here](src/main.rs).

```rs
let x = 20;
// parentheses are optional
if (x < 200) {
  print("TRUE!");
}
```

becomes...

```
1 INF_CMP 0 1 2
2 PRINT 3
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

```
1 INF_CMP 4 0 6
2 ADD 1 2 3
3 MOV 2 1
4 MOV 3 2
5 ADD 4 9 4
6 JMP 5 true
7 PRINT 3
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
You can import functions from other `.spock` files by using the `import` keyword like shown below.
Please note that import statements are directly replaced by the file's contents, and as such error messages will not be
able to specify which file the error comes from.
- `otherfile.spock`:
```rs
fn demo() {
print("Hello World!");
}
```

- `main.spock`:
```rs
import path/to/otherfile.spock;

fn main() {
demo();
}
```
### Arrays
```rs
let x = [0,1,2,3,4];
print(x[0]);
x[1] = "test!";
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
- `Number` (internally an f64 by default)
- `Array` (`[1, 2, 3, "4", 5.0, true]`)
- `String`
#### Converting types
- `x` to `Number`=> `num(x)`
- `x` to `Float`=> `float(x)`
- `x` to `String`=> `str(x)`

## Built-in functions

- `print(<object: String>)` - Prints the given String
- `input(<message: String>) -> String` - Prompt the user for input (with a prompt if given)
- `type(<object: Any>) -> String` - Returns the type of the given object
- `<Number>.abs() -> Number` - Returns the absolute value of the given number
- `<Number>.round() -> Number` - Rounds the given number to the nearest integer (Number type simply returns itself)
- `<Array/String>.len() -> Number` - Returns the length of the given Array (number of elements) or String (number of
  letters)
- `<Number>.sqrt() -> Number` - Returns the square root of the given number
- `the_answer()` - Prints and returns the answer to the Ultimate Question of Life, the Universe, and Everything.
- `range(<start: Integer>, <end: Integer (OPTIONAL)>) -> Array` - Returns an array of integers:
  - `[0..<first argument>]` if only one argument was provided
  - `[<first argument>..<second argument>]` if two arguments were provided
- `io::open(<path: String>)` - Checks if the file path exists and returns a File object.
- `io::delete(<path: String>)` - Deletes the given file path if it exists
- `<File>.write(<contents: String>, <truncate: Boolean (OPTIONAL ==> DEFAULT = false)>)` - Given a File object, writes
  to the file.
- `<File>.read() -> String` - Given a File object, returns the contents of the file.
- `<String>.uppercase() -> String` - Capitalizes all the letters of the given string
- `<String>.lowercase() -> String` - Lowercases all the letters of the given string
- `<Array/String>.contains(<object: Any>) -> Boolean` - Checks if the array contains the given element, or checks if the
  string contains the given substring
- `<String>.trim() -> String` - Removes the leading and trailing whitespace
- `<String>.trim_left() -> String` - Removes the leading whitespace
- `<String>.trim_right() -> String` - Removes the trailing whitespace
- `<String>.trim_sequence(<substring: String>) -> String` - Removes the given substring from the start and end of the
  string
- `<String>.trim_sequence_left(<substring: String>) -> String` - Removes the given substring from the start of the
  string
- `<String>.trim_sequence_right(<substring: String>) -> String` - Removes the given substring from the end of the string
- `<Array/String>.index(<object: Any>) -> Number` - Returns the index in the Array of the given element, or the index in
  the String of the given substring
- `<Array/String>.rindex(<object: Any>) -> Number` - Returns the index in the Array of the given element, or the index
  in the String of the given substring, **starting from the end of the set**
- `<String>.is_num() -> Boolean` - Returns whether the string is a number or not
- `<Array/String>.repeat(<n: Number>) -> Array/String` - Returns an Array or a String with the set itself repeated n
  times.
- `<Array>.push(<object: Any>)` - Appends the given object to the end of the array
