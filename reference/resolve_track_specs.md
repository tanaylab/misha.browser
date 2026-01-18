# Resolve track specifications to extractable expressions

Handles three types of track specifications:

- String: track name, vtrack reference, or expression vtrack

- Object with 'expr': inline track expression

- Object with 'src'/'func': inline vtrack definition

## Usage

``` r
resolve_track_specs(tracks, browser = NULL, cfg = NULL)
```

## Arguments

- tracks:

  List of track specifications

- browser:

  Browser object (for expr_vtracks lookup)

- cfg:

  Browser configuration (for inline vtrack creation)

## Value

List with 'exprs' (expressions to extract) and 'names' (column names)
