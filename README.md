# Rwiring

> **Analysis of structural and weighted changes in ecological
> interaction networks**

Rwiring is an R package for analysing how ecological interaction
networks change across space and time.

The package provides a complete workflow to:

- Prepare and validate interaction datasets.
- Compare any pair of ecological networks.
- Classify node persistence and species turnover.
- Classify interaction persistence, loss, gain, and rewiring.
- Quantify changes in interaction strength.
- Summarize link changes using a unified classification for binary and weighted networks.
- Visualize network changes using interaction or adjacency matrices.

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
        │
        ▼
plot_network_changes_matrix()
```

where:

- `classify_network_nodes()` classifies species as permanent, lost, or gained;
- `classify_network_links()` classifies interaction persistence, loss, gain, and the mechanism responsible for structural change;
- `quantify_network_changes()` combines structural information with the direction of strength change and summarizes the resulting link-change categories (seven for binary networks and nine for weighted networks);
- `plot_network_changes_matrix()` provides a matrix-based visualization of interaction persistence, loss, gain, and, for weighted networks, changes in interaction strength.

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

# Classification of network changes

Rwiring compares two interaction networks and classifies every interaction according to **three complementary dimensions**:

1. **Direction**: whether the interaction persists, disappears, or appears.
2. **Mechanism**: whether structural change is caused by interaction rewiring among permanent species or by species turnover.
3. **Strength direction**: whether interaction strength remains unchanged, decreases, or increases.

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

This node classification is then used to determine the mechanism associated with each interaction.

------------------------------------------------------------------------

## Mechanism

Each interaction is assigned one of four mechanisms.

| Mechanism | Definition |
|-----------|------------|
| `permanent` | Persistent interaction between two permanent nodes. |
| `rewiring` | Lost or gained interaction between two permanent nodes. |
| `one_non_permanent_node` | One interaction partner is lost or gained. |
| `two_non_permanent_nodes` | Both interaction partners are lost or gained. |

## Strength direction

`Strength_direction` describes whether an interaction is unchanged, weakened, or strengthened between the two networks.

| Strength direction | Definition |
|--------------------|------------|
| `unchanged` | A persistent interaction whose strength does not change beyond the specified tolerance. |
| `weakened` | A persistent interaction whose strength decreases, or any lost interaction. |
| `strengthened` | A persistent interaction whose strength increases, or any gained interaction. |

The tolerance used to distinguish unchanged, weakened, and strengthened persistent interactions can be controlled with the `strength_tolerance` argument of `quantify_network_changes()`.

For binary networks, interaction strengths are 0 or 1. Consequently:

- persistent interactions are always `unchanged`;
- lost interactions are always `weakened`;
- gained interactions are always `strengthened`.

# Link-change categories

Rwiring combines `Direction`, `Mechanism`, and `Strength_direction` into a single link-change classification.

## Binary networks

Seven combinations are possible:

| Direction | Mechanism | Strength_direction |
|-----------|-----------|--------------------|
| `persistent` | `permanent` | `unchanged` |
| `lost` | `rewiring` | `weakened` |
| `lost` | `one_non_permanent_node` | `weakened` |
| `lost` | `two_non_permanent_nodes` | `weakened` |
| `gained` | `rewiring` | `strengthened` |
| `gained` | `one_non_permanent_node` | `strengthened` |
| `gained` | `two_non_permanent_nodes` | `strengthened` |

## Weighted networks

When interaction strengths are available, persistent interactions between permanent species can additionally become weaker or stronger. This expands the classification to nine possible categories:

| Direction | Mechanism | Strength_direction |
|-----------|-----------|--------------------|
| `persistent` | `permanent` | `unchanged` |
| `persistent` | `permanent` | `weakened` |
| `persistent` | `permanent` | `strengthened` |
| `lost` | `rewiring` | `weakened` |
| `lost` | `one_non_permanent_node` | `weakened` |
| `lost` | `two_non_permanent_nodes` | `weakened` |
| `gained` | `rewiring` | `strengthened` |
| `gained` | `one_non_permanent_node` | `strengthened` |
| `gained` | `two_non_permanent_nodes` | `strengthened` |

No other combinations are valid.

This unified classification makes the binary case a special case of the weighted framework: binary networks contain seven possible categories, whereas weighted networks contain nine.

------------------------------------------------------------------------

# Weighted change metrics

For weighted networks, the link-level classification additionally includes
quantitative strength-change metrics:

| Variable | Definition |
|----------|------------|
| `Strength_change` | `Strength_network_2 - Strength_network_1`. |
| `Absolute_strength_change` | Absolute value of `Strength_change`. |

The summary table additionally reports, for each link-change category:

- total interaction strength in Network 1;
- total interaction strength in Network 2;
- net strength change;
- total absolute strength change;
- mean strength change per interaction;
- mean absolute strength change per interaction.

Categories containing no interactions have `n_links = 0`. In weighted networks, their aggregated strength metrics are reported as zero and their mean strength-change metrics as `NA`.

------------------------------------------------------------------------

# Worked example

Rwiring includes a synthetic plant–pollinator dataset spanning three years. The dataset was designed to illustrate the different mechanisms of network change, including interaction persistence, rewiring, species turnover, interaction gain and loss, and changes in interaction strength.

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

# Visualize the comparison
plot_network_changes_matrix(changes)
```

------------------------------------------------------------------------

## Exploring the results

The resulting `network_changes` object contains four components:

- `comparison`: metadata describing the comparison;
- `structural`: counts for each valid link-change category and, for weighted networks, aggregated and mean strength-change metrics;
- `links`: interaction-level classifications;
- `nodes`: node-level classifications.

```r
# Print a concise overview
changes

# Display a summary
summary(changes)

# Link-change summary
changes$structural

# Link-level classifications
head(changes$links)

# Node-level classifications
head(changes$nodes)
```

### Species turnover

Node-level classifications can be used to inspect species turnover. For example, the number of permanent, lost, and gained nodes in each guild can be obtained with:

```r
dplyr::count(
  changes$nodes,
  Guild,
  Node_status
)
```

### Interaction rewiring

Link-level classifications can be used to identify the interactions responsible for network reorganization. For example, interactions classified as rewiring can be extracted with:

```r
changes$links |>
  dplyr::filter(Mechanism == "rewiring") |>
  dplyr::select(
    Origin_node,
    Destiny_node,
    Direction,
    Strength_direction
  )
```

### Changes in interaction strength

For weighted networks, the persistent interactions showing the largest changes in strength can be identified with:

```r
changes$links |>
  dplyr::filter(
    Direction == "persistent",
    Strength_direction != "unchanged"
  ) |>
  dplyr::arrange(
    dplyr::desc(Absolute_strength_change)
  ) |>
  dplyr::select(
    Origin_node,
    Destiny_node,
    Strength_network_1,
    Strength_network_2,
    Strength_direction,
    Absolute_strength_change
  ) |>
  head(10)
```
------------------------------------------------------------------------

# Matrix visualization

Network changes can be visualized directly from a `network_changes`
object using `plot_network_changes_matrix()`.

The matrix representation is selected automatically according to network
topology and direction:

-   undirected multipartite networks with exactly two guilds are
    represented as rectangular interaction matrices;
-   unipartite networks are represented as square adjacency matrices;
-   directed multipartite networks and multipartite networks with more
    than two guilds are represented as square adjacency matrices.

Nodes are grouped according to their status in the comparison: permanent
nodes are shown first, followed by lost and gained nodes. Within each
group, nodes are ordered alphabetically. For multipartite adjacency
matrices, guilds form separate alphabetical blocks and node-status
ordering is applied within each guild.

Interaction change is represented by cell colour:

-   grey: persistent interaction;
-   orange: lost interaction;
-   blue: gained interaction.

For persistent interactions in weighted networks, changes in interaction
strength are additionally represented by symbols:

-   upward triangle: strengthened interaction;
-   downward triangle: weakened interaction;
-   no symbol: unchanged interaction.

Lost and gained interactions do not receive strength-change symbols
because their weakening or strengthening is already implied by their
structural change.

## Basic visualization

``` r
plot_network_changes_matrix(changes)
```

For undirected two-guild multipartite networks, the guild assigned to
rows can be selected explicitly:

``` r
plot_network_changes_matrix(
  changes,
  row_guild = "plant"
)
```

Node labels can be hidden for large networks:

``` r
plot_network_changes_matrix(
  changes,
  show_node_labels = FALSE
)
```

## Encoding interaction strength

For weighted networks, interaction magnitude can optionally be
represented by cell opacity:

``` r
plot_network_changes_matrix(
  changes,
  encode_strength = TRUE
)
```

When strength encoding is enabled:

-   persistent links use the mean interaction strength across both
    networks;
-   lost links use their strength in Network 1;
-   gained links use their strength in Network 2.

The minimum opacity can be controlled with `min_alpha`:

``` r
plot_network_changes_matrix(
  changes,
  encode_strength = TRUE,
  min_alpha = 0.2
)
```

A common maximum strength can also be supplied when comparing several
plots:

``` r
plot_network_changes_matrix(
  changes,
  encode_strength = TRUE,
  strength_max = 20
)
```

Using the same `strength_max` across plots makes opacity directly
comparable between network comparisons.

------------------------------------------------------------------------

## Citation

If you use **Rwiring**, please cite the accompanying methodological
publication once available.

------------------------------------------------------------------------

## Development status

Rwiring is under active development.

Bug reports, feature requests and pull requests are welcome.
