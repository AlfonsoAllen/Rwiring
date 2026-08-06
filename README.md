# Rwiring

> **Analysis of structural and weighted changes in ecological
> interaction networks**

Rwiring is an R package for analysing how ecological interaction
networks change across space and time.

The package provides a complete workflow to:

-   Prepare and validate interaction datasets.
-   Compare any pair of ecological networks.
-   Classify node persistence and interaction rewiring.
-   Quantify structural changes.
-   Quantify changes in interaction strength for weighted networks.

Rwiring supports:

-   Weighted or binary networks
-   Directed or undirected networks
-   Unipartite or multipartite networks
-   Temporal, spatial and spatiotemporal comparisons

------------------------------------------------------------------------

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("JJ-Lab/Rwiring")
library(Rwiring)
```

------------------------------------------------------------------------

## Workflow

``` text
Interaction table
        │
        ▼
prepare_interactions()
        │
        ▼
classify_network_nodes()
        │
        ▼
classify_network_links()
        │
        ▼
quantify_network_changes()
```

------------------------------------------------------------------------

## Input data

Each row represents one interaction.

  Site   Time   Origin_node   Destiny_node   Strength
  ------ ------ ------------- -------------- ----------
  A      2020   Plant_1       Bee_1          3
  A      2020   Plant_1       Bee_2          1
  A      2020   Plant_2       Bee_1          2

Optional columns:

-   `Origin_guild`
-   `Destiny_guild`

------------------------------------------------------------------------

## Quick start

``` r
library(Rwiring)

prepared <- prepare_interactions(
    synthetic_interactions,
    weighted = TRUE,
    multipartite = TRUE
)

changes <- quantify_network_changes(
    prepared,
    site_1 = "Site_1",
    time_1 = 2020,
    site_2 = "Site_1",
    time_2 = 2021
)

summary(changes)
```

Inspect results:

``` r
changes$nodes
changes$links
changes$structural
```

------------------------------------------------------------------------

## Structural classification

Each interaction is classified according to:

### Direction

-   `persistent`
-   `lost`
-   `gained`

### Mechanism

-   `permanent`
-   `rewiring`
-   `one_non_permanent_node`
-   `two_non_permanent_nodes`

------------------------------------------------------------------------

## Weighted networks

For weighted networks, Rwiring additionally computes:

-   `Strength_change`
-   `Absolute_strength_change`
-   `Strength_direction`
-   Total strength changes
-   Mean strength changes

Persistent links are classified as:

-   strengthened
-   weakened
-   unchanged

using a user-defined tolerance.

------------------------------------------------------------------------

## Example dataset

Rwiring includes a synthetic dataset covering every structural rewiring
category.

``` r
data(synthetic_interactions)

head(synthetic_interactions)
```

------------------------------------------------------------------------

## Typical output

``` r
summary(changes)

changes$structural
changes$links
changes$nodes
```

------------------------------------------------------------------------

## Citation

If you use **Rwiring**, please cite the accompanying methodological
publication once available.

------------------------------------------------------------------------

## Development status

Rwiring is under active development.

Bug reports, feature requests and pull requests are welcome.
