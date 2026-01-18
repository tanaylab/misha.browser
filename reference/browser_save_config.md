# Save configuration to YAML file

Saves the browser configuration to a YAML file. Internal fields (those
starting with `._`) are automatically removed before saving, so the
resulting file is clean and portable.

## Usage

``` r
browser_save_config(cfg, file)
```

## Arguments

- cfg:

  Configuration list (typically from a browser object via `browser$cfg`)

- file:

  Output file path (should end with .yaml or .yml)

## Value

Invisibly returns the original configuration (for piping)

## Details

The saved configuration can be loaded later with
[`browser_load_config`](https://tanaylab.github.io/misha.browser/reference/browser_load_config.md)
or used directly with
[`browser_create`](https://tanaylab.github.io/misha.browser/reference/browser_create.md).

Internal fields that are removed include:

- `._config_file`: Original config file path

- `._misha_root`: Resolved misha database path

- `._profile`: Active profile name

- `._cache_signature`: Pre-computed cache signatures

## See also

[`browser_load_config`](https://tanaylab.github.io/misha.browser/reference/browser_load_config.md)
to load configuration,
[`browser_create_config`](https://tanaylab.github.io/misha.browser/reference/browser_create_config.md)
to create configuration programmatically

## Examples

``` r
if (FALSE) { # \dontrun{
# Save browser configuration
browser_save_config(browser$cfg, "my_browser.yaml")

# Modify and save
browser <- browser_add_panel(browser, name = "new_panel", tracks = "my_track")
browser_save_config(browser$cfg, "updated_config.yaml")
} # }
```
