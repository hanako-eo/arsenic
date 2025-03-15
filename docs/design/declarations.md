Like all other languages, Arsenic allows you to define and declare variables and functions. As in modern JS, Arsenic will use the `let` and `const` keywords to define mutable and immutable variables respectively. But for functions, the function definition keyword will start with `func`.

```ars
// all types are inferred
let a = 0;
const b = 0;

func c() {
    return 0;
}
```

## `const` difference between JS and Arsenic

However, the behaviour of const variables will not be the same as in basic JS. In JS write:
```js
const my_var = {
    inner_key: 0,
};

// allow
my_var.inner_key = 1;
```
is possible, but it will violate the principle of immutability and can have strong side effects. In arsenic, it will not be possible to write such code and it will return an error at compile time.

## Differences between JS and Arsenic functions

To avoid mutability problems, function arguments are considered to be const by default and can therefore take anything as a parameter. But, for example, if there is a need to take a mutable argument, it will then be possible to modify the parameter and declare that the argument is mutable, and then immutable parameters will no longer be possible to pass without having an error at compilation time.

```ars
func factorial(n: int) -> int {
    // do n = 0 will produce an error
    return n * factorial(n - 1);
}

func factorial2(let n: int) -> int {
    // do n = 0 is correct
    return n * factorial2(n - 1);
}

const n = 7;
factorial(n); // ok
factorial2(n); // not ok (in part)
```
