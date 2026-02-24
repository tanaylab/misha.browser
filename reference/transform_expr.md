# Custom expression transform

Evaluates a custom R expression on the data. The expression has access
to variables `x` (the data vector) and `pos` (position vector).

## Usage

``` r
transform_expr(x, pos, params)
```

## Arguments

- x:

  Numeric vector

- pos:

  Position vector

- params:

  List with: expr (R expression as string)

## Value

Transformed vector

## Security Warning

This function evaluates R expressions from YAML configuration files
using [`eval()`](https://rdrr.io/r/base/eval.html). The evaluation
environment is restricted to a whitelist of safe math and vector
functions (no file I/O, system calls, or environment access).
Nevertheless, **only load configuration files from trusted sources**.
