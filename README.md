# Production-Data-Analysis
Acoustic speech production analysis using linear mixed-effects models and DBSCAN clustering in R

# Bengali Stop Production Analysis

Statistical and unsupervised-learning analyses of acoustic measurements from Bengali stop production.

The project examines variation in laryngeal stop categories across **place of articulation** and **phonetic context**.

## Acoustic Measures

The production dataset contains three principal acoustic dimensions:

- **VOT** — Voice Onset Time
- **CV** — Closure Voicing
- **CD** — Closure Duration

## Files

### `production_mixed_effects_analysis.R`

Fits linear mixed-effects models to the production data.

Separate models are estimated for:

- Initial bilabials
- Medial bilabials
- Initial velars
- Medial velars

The primary analyses examine **VOT** and **closure voicing**, with stimulus-speaker gender included as a fixed effect and participant included as a random intercept.

Post-hoc comparisons are performed using estimated marginal means.

The repository also includes full models testing:

**Stop Category × Context × Place of Articulation**

while controlling for stimulus-speaker gender.

### `production_dbscan_clustering.R`

Uses **DBSCAN unsupervised clustering** to explore whether productions form acoustically distinct groupings in three-dimensional phonetic space.

Clustering is performed using standardized:

- VOT
- Closure duration
- Closure voicing

Separate analyses are conducted for initial and medial bilabials and velars.

The workflow includes:

- Standardization of acoustic variables
- k-nearest-neighbor distance inspection
- Epsilon parameter exploration
- DBSCAN clustering
- Cluster-by-stimulus comparisons
- Interactive 3D visualization of acoustic space

## Methods and Skills Demonstrated

- Linear mixed-effects modelling
- Random-effects modelling
- Model interactions
- Estimated marginal means and Tukey-adjusted comparisons
- Acoustic phonetic analysis
- Unsupervised machine learning
- DBSCAN clustering
- Feature scaling
- Parameter exploration
- Three-dimensional data visualization
- Data preprocessing and factor coding in R

## Research Context

The analyses examine the acoustic organization of Bengali laryngeal stop categories and how cues such as voice onset time and closure voicing vary across phonetic environments.

## Software

Analyses were conducted in **R**, using packages including:

- `lme4`
- `lmerTest`
- `emmeans`
- `dplyr`
- `dbscan`
- `plotly`
- `ggplot2`
