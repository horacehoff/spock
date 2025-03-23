# Spock
A work-in-progress programming language written in Rust for the best performance possible, whose syntax takes inspiration from Rust and Python. Basically, its goal is to provide a faster alternative to Python, and one that's closer to low-level languages, while still being accessible to a wide audience.

Key info:

- ~5-10x faster than Python in most cases, can go as far as :
  - Fibonacci function: ~14000x faster(first run) or ~2000x faster (later runs)

## Instruction Set

Spock uses an instruction set with a size of 8 bytes.
The available instructions can be seen [here](src/main.rs).

```
let x = 20;
// parentheses are optional
if (x < 200) {
  print("TRUE!");
}
```

becomes...

```
1 INF 0 1 2
2 CMP 2 2
3 PRINT 3
```

---

```
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
1 INF 4 0 5
2 CMP 5 6
3 ADD 1 2 3
4 MOV 2 1
5 MOV 3 2
6 ADD 4 9 4
7 JMP 6 true
8 PRINT 3
```

## Syntax examples
```
let x = 20;
// parentheses are optional
if (x == 20) {
  print("TRUE!");
}
```
```
let x = 0;
// parentheses are optional
while (x < 10) {
  print(x);
  x = x+1;
}
```
```
// parentheses are optional
for (x in [1, 2, 3]) {
  for y in "abc" {
    print(x.toStr()+y);
  }
}
```
```
let x = [10, 20, 30, 40];
print(x[0]);
```

## Types
- `Boolean` (`true`/`false`)
- `Integer`
- `Array` (`[1, 2, 3, "4", 5.0, true]`)
- `String`
- `Float`
#### Converting types
- `x` to `Integer`=> `int(x)`
- `x` to `Float`=> `float(x)`
- `x` to `String`=> `str(x)`

## Built-in functions
- `print(1, String)` - Prints the given String
- `input(0/1, String) -> String` - Prompt the user for input (with a prompt if given)
- `type(1, Any) -> String` - Returns the type of the given object
- `hash(1, Any) -> String` - Returns a hash of the given object using the BLAKE3 hash function
- `abs(1, Integer/Float) -> Integer/Float` - Returns the absolute value of the given number
- `round(1, Integer/Float) -> Integer` - Rounds the given number to the nearest integer (Integer type simply returns itself)
- `len(1, Array/String) -> Integer` - Returns the length of the given Array (number of elements) or String (number of letters)
- `sqrt(1, Integer/Float) -> Integer/Float` - Returns the square root of the given number
- `the_answer()` - Prints and returns the answer to the Ultimate Question of Life, the Universe, and Everything.
- `range(1/2/3, Integer)` - Returns an array of integers:
  - `[0..<first argument>]` if only one argument was provided
  - `[<first argument>..<second argument>]` if two arguments were provided
  - `[<first argument>..<second argument>]`, with the step defined by the third argument, if three arguments were provided

## Basic macros
You can define macros outside functions using the following syntax:
`replace name->value`\
**Example**:
```
replace hello->goodbye

func main() {
print("hello world");
}
```

## Imports
You can import functions from other `.spock` files by using the `import` keyword like so:
- `otherfile.spock`:
```
func demo() {
print("Hello World!");
}
```

- `main.spock`:
```
import path/to/otherfile.spock;

func main() {
demo();
}
```

## Files
```
print(io::open("myfile.txt").read());
let file = io::open("myfile.txt")
// overwrite the file's contents
file.write("new content");
// append to the end of the file
file.append("appended content");

```
