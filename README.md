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

# Workflow

The complete workflow implemented by Rwiring is:

```r
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

where:

- `classify_network_nodes()` classifies species as permanent, lost, or gained;
- `classify_network_links()` assigns every interaction to one of the seven
  structural categories;
- `quantify_network_changes()` summarizes the structural reorganization and,
  for weighted networks, quantifies changes in interaction strength.

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

# Structural classification

Rwiring compares two interaction networks and classifies every interaction
according to **two complementary dimensions**:

1. **Direction**: whether the interaction persists, disappears, or appears.
2. **Mechanism**: whether the change is explained by species turnover or by
   interaction rewiring among persistent species.

The comparison is always **directional**, from **Network 1** to **Network 2**.

## Direction

Each interaction belongs to one of three categories:

| Direction | Definition |
|-----------|------------|
| `persistent` | Present in both networks. |
| `lost` | Present only in Network 1. |
| `gained` | Present only in Network 2. |

------------------------------------------------------------------------

## Node classification

Before classifying interactions, every node is classified as:

| Node status | Definition |
|-------------|------------|
| `permanent` | Present in both networks. |
| `lost` | Present only in Network 1. |
| `gained` | Present only in Network 2. |

This node classification is then used to determine the ecological mechanism
behind each interaction.

------------------------------------------------------------------------

## Mechanism

Each interaction is assigned one of four mechanisms.

| Mechanism | Definition |
|-----------|------------|
| `permanent` | Persistent interaction between two permanent nodes. |
| `rewiring` | Lost or gained interaction between two permanent nodes. |
| `one_non_permanent_node` | One interaction partner is lost or gained. |
| `two_non_permanent_nodes` | Both interaction partners are lost or gained. |

The seven valid structural categories are therefore:

| Direction | Mechanism |
|-----------|-----------|
| `persistent` | `permanent` |
| `lost` | `rewiring` |
| `lost` | `one_non_permanent_node` |
| `lost` | `two_non_permanent_nodes` |
| `gained` | `rewiring` |
| `gained` | `one_non_permanent_node` |
| `gained` | `two_non_permanent_nodes` |

No other combinations are allowed.

------------------------------------------------------------------------

# Weighted networks

When interaction strengths are available, Rwiring additionally quantifies how
interaction intensity changes between the two networks.

For every interaction, the following quantities are calculated:

| Variable | Definition |
|----------|------------|
| `Strength_network_1` | Interaction strength in Network 1. |
| `Strength_network_2` | Interaction strength in Network 2. |
| `Strength_change` | `Strength_network_2 - Strength_network_1`. |
| `Absolute_strength_change` | Absolute value of `Strength_change`. |
| `Strength_direction` | Qualitative description of the strength change. |

Persistent interactions are classified as:

- `strengthened`
- `weakened`
- `unchanged`

according to the user-defined `strength_tolerance`.

Lost interactions are always classified as **weakened**, whereas gained
interactions are always classified as **strengthened**.

The structural summary additionally reports, for each structural category:

- total interaction strength in each network,
- net strength change,
- total absolute strength change,
- mean strength change,
- mean absolute strength change.

------------------------------------------------------------------------

## Worked example

Rwiring includes a synthetic plant–pollinator dataset spanning three years.
The dataset was designed to contain examples of all structural change
categories (persistent interactions, rewiring, species turnover, interaction
gain, and interaction loss), making it suitable for learning the package
workflow.

```r
library(Rwiring)

# Load the example dataset
data(synthetic_interactions)

# Inspect its structure
head(synthetic_interactions)

# Rename the columns to the format expected by Rwiring
interactions <- synthetic_interactions

names(interactions) <- c(
  "Site",
  "Time",
  "Origin_node",
  "Destiny_node",
  "Strength"
)

# Add guild information (optional, but recommended)
interactions$Origin_guild <- "plant"
interactions$Destiny_guild <- "pollinator"

# Prepare the interaction table
prepared <- prepare_interactions(
  interactions,
  directed = FALSE,
  multipartite = TRUE
)

summary(prepared)

# Compare two annual networks
changes <- quantify_network_changes(
  prepared,
  site_1 = "Synthetic_meadow",
  time_1 = 2018,
  site_2 = "Synthetic_meadow",
  time_2 = 2019
)
```

---

## Exploring the results

The resulting object contains four components:

- `comparison`: metadata describing the comparison;
- `structural`: summary of structural changes;
- `links`: interaction-level classifications;
- `nodes`: node-level classifications.

```r
# Print a concise overview
changes

# Display a summary
summary(changes)

# Structural summary
changes$structural

# Link-level classifications
head(changes$links)

# Node-level classifications
head(changes$nodes)
```
------------------------------------------------------------------------

## Citation

If you use **Rwiring**, please cite the accompanying methodological
publication once available.

------------------------------------------------------------------------

## Development status

Rwiring is under active development.

Bug reports, feature requests and pull requests are welcome.
