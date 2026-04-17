# File System library

> T represents any type.\
> Array\<T\> represents an array containing elements of type T\
> \<T\> represents any element of type T

## Read

`fs::read(p: String) -> String`\
Returns the contents of the file with path `p`.
Example:
```
print(fs::read("hello_world.txt"));
```

## Exists

`fs::exists(p: String) -> String`\
Returns whether or not the file `p` exists.
Example:
```
print(fs::exists("exists.txt")); // returns true
print(fs::exists("does_not_exist.txt")) // returns false
```

## Write

`fs::write(path: String, contents: String)`\
Writes `contents` to `path`. Creates `path` if it doesn't exist.
Example:
```
fs::write("test.txt", "Hello, World!");
```

## Append

`fs::write(path: String, contents: String)`\
Appends `contents` to `path`. Creates `path` if it doesn't exist.
Example:
```
fs::append("test.txt", "Hello, World!");
```