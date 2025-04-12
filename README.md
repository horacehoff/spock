# Spock
A work-in-progress programming language written in Rust for the best performance possible, whose syntax takes inspiration from Rust and Python. Basically, its goal is to provide a faster alternative to Python, and one that's closer to low-level languages, while still being accessible to a wide audience.

Key info:

- ~5-10x faster than Python in most cases.
  Comparisons [here](COMPARISONS.md)
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
1 INF_CMP 0 1 2
2 PRINT 3
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
1 INF_CMP 4 0 6
2 ADD 1 2 3
3 MOV 2 1
4 MOV 3 2
5 ADD 4 9 4
6 JMP 5 true
7 PRINT 3
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
for x in [1, 2, 3] {
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
- `Number` (internally an f64)
- `Array` (`[1, 2, 3, "4", 5.0, true]`)
- `String`
#### Converting types
- `x` to `Number`=> `num(x)`
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


## Imports
You can import functions from other `.spock` files by using the `import` keyword like shown below.
Please that imports are directly replaced by the file's content, as such error messages will not be able to specify which file the error comes from.
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