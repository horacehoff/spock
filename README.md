# Compute
> Very WIP

- Inspired by Rust & Python
- ~5-6x faster than Python
- Does NOT support nested functions
- Terrible error handling
- Full of bugs
- Supports importing functions from other files

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