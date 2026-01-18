# Deploy a misha.browser app to a local directory

Copies the bundled `app.R` and optionally a config file to a target
directory so it can be served by Shiny Server.

## Usage

``` r
browser_deploy_local(
  dest_dir,
  config = NULL,
  config_name = "config.yaml",
  profile = "server",
  overwrite = FALSE,
  write_env = TRUE,
  extra_files = NULL,
  touch_restart = TRUE
)
```

## Arguments

- dest_dir:

  Target directory to deploy into.

- config:

  Path to a YAML config file to copy. When NULL, no config is copied.

- config_name:

  Filename to use in the destination (default: "config.yaml").

- profile:

  Profile name to set in the deployment environment (default: "server").

- overwrite:

  Logical, overwrite existing files if TRUE.

- write_env:

  Logical, write a `.Renviron` with MISHA_BROWSER\_\* vars.

- extra_files:

  Optional character vector of file paths to copy as-is.

- touch_restart:

  Logical, create or update `restart.txt` in destination.

## Value

Invisible list with deployed paths.
