# Convert misha.vis configuration to misha.browser YAML

Attempts to convert a misha.vis configuration list to a misha.browser
YAML configuration file.

## Usage

``` r
browser_convert_vis_config(
  vis_config,
  output_file = "browser_config.yaml",
  misha_root = NULL
)
```

## Arguments

- vis_config:

  misha.vis configuration list (from vis_create or YAML)

- output_file:

  Output YAML file path

- misha_root:

  Optional misha root path

## Value

Invisibly returns the converted configuration

## Examples

``` r
if (FALSE) { # \dontrun{
# If you have a misha.vis config
old_config <- yaml::read_yaml("vis_config.yaml")
browser_convert_vis_config(old_config, "browser_config.yaml")
} # }
```
