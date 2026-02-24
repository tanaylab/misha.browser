# Safe environment for expression evaluation

Contains only whitelisted math/vector functions. No file I/O, no system
calls, no environment access. This is created once and reused.

## Usage

``` r
.expr_safe_env
```

## Format

An object of class `environment` of length 53.
