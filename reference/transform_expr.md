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

## Security Note

This function evaluates arbitrary R expressions from configuration
files. While the evaluation environment is restricted to base R
functions, configuration files should only be loaded from trusted
sources.
