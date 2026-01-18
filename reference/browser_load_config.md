# Load browser configuration from YAML file

Loads and validates a YAML configuration file for the genome browser.
The configuration can contain multiple profiles (e.g., "local" and
"server") with different misha database paths and settings. If no
profile is specified, the function auto-detects based on which paths
exist.

## Usage

``` r
browser_load_config(file, profile = NULL)
```

## Arguments

- file:

  Path to YAML configuration file

- profile:

  Profile name to use (default: auto-detect based on available paths).
  Common profiles are "local" and "server".

## Value

Parsed and validated configuration list with resolved paths and
defaults. Internal fields (prefixed with `._`) contain resolved paths
and metadata.

## Details

The configuration file should contain:

- `profiles`: Environment-specific settings (misha_root, base_dir,
  data_dir)

- `panels`: Panel definitions for data visualization

- `vtracks`: Virtual track definitions

- `vlines`: Vertical line annotations

- `plot`: Plot settings (iterator, extraction_mode, theme)

- `ui`: User interface settings (title, defaults)

- `navigator`: Gene navigator configuration

- `start`: Initial region settings

## See also

[`browser_save_config`](https://tanaylab.github.io/misha.browser/reference/browser_save_config.md)
to save configuration,
[`browser_create_config`](https://tanaylab.github.io/misha.browser/reference/browser_create_config.md)
to create configuration programmatically,
[`browser_create`](https://tanaylab.github.io/misha.browser/reference/browser_create.md)
to create a browser from configuration

## Examples

``` r
if (FALSE) { # \dontrun{
# Load with auto-detected profile
cfg <- browser_load_config("my_browser.yaml")

# Load with specific profile
cfg <- browser_load_config("my_browser.yaml", profile = "server")

# Use loaded config to create browser
browser <- browser_create(config = "my_browser.yaml")
} # }
```
