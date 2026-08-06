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
remotes::install_github("AlfonsoAllen/Rwiring")
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

# Input data

Rwiring uses a simple edge-list representation of interaction networks, where **each row corresponds to one interaction between two nodes**.

The minimum information required is the identity of the network (`Site` and `Time`) together with the two interacting nodes (`Origin_node` and `Destiny_node`).

For weighted networks, an additional `Strength` column can be supplied. If this column is omitted, the network is automatically treated as **binary**, with all interactions assumed to have equal weight.

Guild information is optional but recommended for multipartite networks, as it facilitates interpretation and downstream analyses.

## Required and optional columns

| Column | Required | Description |
|:-------|:--------:|:------------|
| `Site` | ✓ | Network location identifier. |
| `Time` | ✓ | Network time identifier. |
| `Origin_node` | ✓ | First interaction partner. |
| `Destiny_node` | ✓ | Second interaction partner. |
| `Strength` | Optional | Interaction weight. If omitted, the network is treated as binary. |
| `Origin_guild` | Optional | Guild of the origin node. |
| `Destiny_guild` | Optional | Guild of the destination node. |

## Example

```{r}
interactions <- data.frame(
  Site = c("Site_1", "Site_1", "Site_1"),
  Time = c(2020, 2020, 2021),
  Origin_node = c("Plant_1", "Plant_2", "Plant_1"),
  Destiny_node = c("Bee_1", "Bee_2", "Bee_1"),
  Strength = c(5, 2, 7),
  Origin_guild = c("Plant", "Plant", "Plant"),
  Destiny_guild = c("Pollinator", "Pollinator", "Pollinator")
)
interactions
```

## Notes

- `Site` and `Time` jointly identify a network.
- Multiple interactions may belong to the same network.
- Node identifiers can be either character strings or numeric values.
- Interaction strengths must be non-negative.
- When `Strength` is omitted, all interactions are assumed to have weight 1.
- Guild columns are optional and ignored for unipartite networks.

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
library(Rwiring)

data(synthetic_interactions)

# Inspect the original column names
head(synthetic_interactions)

interactions <- synthetic_interactions

names(interactions) <- c(
  "Site",
  "Time",
  "Origin_node",
  "Destiny_node",
  "Strength"
)

interactions$Origin_guild <- "plant"
interactions$Destiny_guild <- "pollinator"

head(interactions)

prepared <- prepare_interactions(
  interactions,
  directed = FALSE,
  multipartite = TRUE
)

prepared
summary(prepared)

changes <- quantify_network_changes(
  prepared,
  site_1 = "Synthetic_meadow",
  time_1 = 2018,
  site_2 = "Synthetic_meadow",
  time_2 = 2019
)

changes
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
