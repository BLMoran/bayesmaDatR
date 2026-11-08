# Domain columns for a risk-of-bias tool

Domain columns for a risk-of-bias tool

## Usage

``` r
rob_domains(rob_tool = "rob2", include_overall = TRUE, include_optional = TRUE)
```

## Arguments

- rob_tool:

  Tool identifier from
  [`rob_tools()`](https://blmoran.github.io/bayesmaDatR/reference/rob_tools.md).

- include_overall:

  Include the tool's overall judgement column(s).

- include_optional:

  Include design-specific optional domains (RoB 2 `D1b`/`DS`, CLARITY
  `D11`/`D12`, RoB NMA `R1`-`R3`, PROBAST `A1`-`A3`).

## Value

Character vector of column names.
