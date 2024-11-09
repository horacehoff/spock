# Compute
A work-in-progress programming language written in Rust for the best performance possible, whose syntax takes inspiration from Rust and Python.

Key facts:
- ~5-6x faster than Python
- Does not support nested functions
- Terrible error handling
  - No line number
  - May report the wrong error (a syntax error could make the parser think the error's origin is elsewhere)
- Many bugs
- Supports basic macros

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
- `Integer`
- `Array` (`[1, 2, 3, "4", 5.0, true]`)
- `String`
- `Float`

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
You can import functions from other `.compute` files by using the `import`keyword like so:
- `otherfile.compute`:
```
func demo() {
print("Hello World!");
}
```
- `main.compute`:
```
import otherfile; // if the imported file is in a folder, write '<path>/otherfile'

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