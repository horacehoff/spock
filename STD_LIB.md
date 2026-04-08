# Standard library

> T represents any type.\
> Array\<T\> represents an array containing elements of type T\
> \<T\> represents any element of type T

## Print

`print(T)`\
Used to print anything.
Example:
```
print("Hello, World!");
print([42]);
```


## Type

`type(T) -> String`\
Returns the type of the object as a string.
Example:
```
type("Hello, World!"); // Returns "String"
type([42]); // Returns "Array<Integer>"
```

## Float

`float(String | Int) -> Float`\
Returns the string or integer interpreted as a float. It will crash the program at runtime if the given string cannot be converted into a float.
Example:
```
float(42); // Returns 42.0
float("42"); // Returns 42.0
float("Hello, World!"); // Crashes
```

## Int

`int(String | Float) -> Int`\
Returns the string or float interpreted as an integer. It will crash the program at runtime if the given string cannot be converted into an integer.
Example:
```
int(42.0); // Returns 42
int("42"); // Returns 42
int("Hello, World!"); // Crashes
```

## Str

`str(T) -> String`\
Returns the given object as a string.
Example:
```
str(42); // Returns "42"
str([0,1,2,3]); // Returns "[0,1,2,3]"
```


## Bool

`bool(s: String) -> Bool`\
Returns `s` interpreted as a boolean. It will crash the program at runtime if `s` cannot be converted into a boolean.
Example:
```
bool("true"); // Returns true
bool("42"); // Crashes
```

## Input

`input([OPTIONAl]p: String) -> String`\
Asks the user for input.
If provided, it will print `p` prompt before asking.

## Range

`range([OPTIONAL]i: Int, j: Int) -> Array<Int>`\
Returns an array containing the numbers from 0 or `i` to `j`-1.
Example:
```
range(5); // Returns [0,1,2,3,4]
bool(1,5); // Returns [1,2,3,4]
```

## TheAnswer

`the_answer() -> Int`\
Prints "The answer to the Ultimate Question of Life, the Universe, and Everything is 42." and returns the integer 42.

## Uppercase

`<String>.uppercase() -> String`\
Returns the given string as uppercase.

## Lowercase

`<String>.lowercase() -> String`\
Returns the given string as lowercase.

## Len

`<String | Array<T>>.len() -> Int`\
Returns the length of the given collection.

## Contains

`<String>.contains(e: String) -> Bool`\
`<Array<T>>.contains(e: T) -> Bool`\
Returns a bool depicting whether or not the collection contains `e`.

## Trim

`<String>.trim() -> String`\
Returns the given string, trimmed (leading and trailing whitespace removed).

## TrimLeft

`<String>.trim_left() -> String`\
Returns the given string, with the left trimmed (leading whitespace removed).

## TrimRight

`<String>.trim_right() -> String`\
Returns the given string, with the right trimmed (trailing whitespace removed).

## TrimSequence

`<String>.trim_sequence(s: String) -> String`\
Returns the given string, with `s` removed from the start and end of the string.

## TrimSequenceLeft

`<String>.trim_sequence_left(s: String) -> String`\
Returns the given string, with `s` removed from the start of the string.

## TrimSequenceRight

`<String>.trim_sequence_right(s: String) -> String`\
Returns the given string, with `s` removed from the end of the string.

## Find

`<String>.find(e: String) -> Int`\
`<Array<T>>.find(e: T) -> Int`\
Returns the index of the `e` in a collection. If the element isn't found, it will return `-1`.

## Repeat

`<String>.repeat(n: Int) -> String`\
`<Array<T>>.repeat(n: Int) -> Array<T>`\
Returns a collection repeated n times.

## Push

`<Array<T>>.push(e: T)`\
Adds `e` to the end of an array.

## Remove

`<Array<T>>.remove(n: Int)`\
Removes the n-th element from an array.

## Sqrt

`<Float>.sqrt() -> Float`\
Returns the square root of a float.

## Round

`<Float>.round() -> Float`\
Rounds a float to the nearest integer

## Floor

`<Float>.floor() -> Float`\
Floors a float.

## Abs

`<Float>.abs() -> Float`\
`<Int>.abs() -> Int`\
Returns the absolute value of a number.

## IsFloat

`<String>.is_float() -> Bool`\
Returns whether or not a string represents a float.

## IsInt

`<String>.is_int() -> Bool`\
Returns whether or not a string represents an integer.

## Reverse

`<Array<T>>.reverse() -> Array<T>`\
`<String>.reverse() -> String`\
Reverses a collection.

## Split

`<Array<T>>.split(e: T) -> Array<T>`\
`<String>.split(e: String) -> String`\
Splits a collection with the given separator `e`.

## StartsWith

`<String>.starts_with(s: String) -> Bool`\
Returns whether or not the given string starts with `s`.

## EndsWith

`<String>.ends_with(s: String) -> Bool`\
Returns whether or not the given string ends with `s`.

## Replace

`<String>.replace(a: String, b: String) -> String`\
Returns the given string with all occurences of `a` replaced with `b`.
