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

## Imports
You can import functions from other `.spock` files by using the `import` keyword like shown below.
Please note that import statements are directly replaced by the file's contents, and as such error messages will not be
able to specify which file the error comes from.
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