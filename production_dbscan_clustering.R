# Production Study: DBSCAN Clustering Analysis
# --------------------------------------------
# Cleaned and consolidated from the original bilabial and velar R console
# analyses.
#
# Acoustic dimensions:
#   VOT = voice onset time
#   CD  = closure duration
#   CV  = closure voicing
#
# Analyses:
#   1. Initial bilabials
#   2. Medial bilabials
#   3. Initial velars
#   4. Medial velars
#
# Each analysis:
#   - subsets the production data
#   - standardizes VOT, CD, and CV
#   - optionally inspects k-NN distances / candidate eps values
#   - runs DBSCAN
#   - creates an interactive 3-D plot
#   - prints a stimulus-by-cluster table
#
# Input file expected:
#   Production_Data_R_code.txt
#
# Packages:
#   dbscan, plotly, ggplot2, dplyr, stringr


# Load packages -------------------------------------------------------------

library(dbscan)
library(plotly)
library(ggplot2)
library(dplyr)
library(stringr)


# Load data -----------------------------------------------------------------

ProdData <- read.table(
  "Production_Data_R_code.txt",
  header = TRUE
)


# Derive analysis variables -------------------------------------------------
# These variables were derived from the stimulus labels in the production
# analysis. This keeps the clustering script runnable even when the input
# file contains only the original stimulus labels and acoustic measurements.

ProdData <- ProdData %>%
  mutate(
    BaseStimuli = str_remove(Stimuli, "-[mf]$"),

    StimGender = case_when(
      str_ends(Stimuli, "-m") ~ "m",
      str_ends(Stimuli, "-f") ~ "f",
      TRUE ~ NA_character_
    ),

    Context = case_when(
      BaseStimuli %in% c(
        "ba", "bha", "pa", "pha",
        "ga", "gha", "ka", "kha"
      ) ~ "Initial",

      BaseStimuli %in% c(
        "aba", "abha", "apa", "apha",
        "aga", "agha", "aka", "akha"
      ) ~ "Medial",

      TRUE ~ NA_character_
    ),

    Place = case_when(
      BaseStimuli %in% c(
        "ba", "bha", "pa", "pha",
        "aba", "abha", "apa", "apha"
      ) ~ "Bilabial",

      BaseStimuli %in% c(
        "ga", "gha", "ka", "kha",
        "aga", "agha", "aka", "akha"
      ) ~ "Velar",

      TRUE ~ NA_character_
    )
  )


# Helper function: inspect candidate eps values -----------------------------

inspect_eps <- function(data_scaled, eps_candidates, minPts = 6) {

  k <- minPts - 1

  knn_dist <- kNNdist(
    data_scaled,
    k = k
  )

  sorted_knn_dist <- sort(knn_dist)

  knn_plot_data <- data.frame(
    index = seq_along(sorted_knn_dist),
    distance = sorted_knn_dist
  )

  print(
    ggplot(
      knn_plot_data,
      aes(x = index, y = distance)
    ) +
      geom_line(linewidth = 1) +
      labs(
        title = paste(
          "k-NN Distance Plot (k =",
          k,
          ") for Epsilon Selection"
        ),
        x = "Data Points (Sorted by Distance)",
        y = paste(
          "Distance to",
          k,
          "-th Nearest Neighbor"
        )
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  )

  for (eps_candidate in eps_candidates) {

    result <- dbscan(
      data_scaled,
      eps = eps_candidate,
      minPts = minPts
    )

    n_clusters <- length(
      unique(result$cluster)
    ) - 1

    n_noise <- sum(
      result$cluster == 0
    )

    cat(
      sprintf(
        "eps = %.1f: %d clusters, %d noise points\n",
        eps_candidate,
        n_clusters,
        n_noise
      )
    )
  }
}


# Helper function: run and visualize DBSCAN ---------------------------------

run_dbscan_analysis <- function(
  subset_data,
  eps_value,
  cluster_labels,
  plot_title,
  minPts = 6
) {

  acoustic_data <- subset_data[
    ,
    c("VOT", "CD", "CV")
  ]

  acoustic_scaled <- scale(
    acoustic_data
  )

  dbscan_result <- dbscan(
    acoustic_scaled,
    eps = eps_value,
    minPts = minPts
  )

  acoustic_data$cluster <- factor(
    dbscan_result$cluster
  )

  # Preserve the original manually assigned cluster labels.
  levels(acoustic_data$cluster) <- cluster_labels[
    seq_along(
      levels(acoustic_data$cluster)
    )
  ]

  plot_3d <- plot_ly(
    acoustic_data,
    x = ~VOT,
    y = ~CD,
    z = ~CV,
    color = ~cluster,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 4,
      opacity = 0.8
    ),
    text = ~paste(
      "VOT:", round(VOT, 3),
      "<br>CD:", round(CD, 3),
      "<br>CV:", round(CV, 3)
    )
  ) %>%
    layout(
      title = plot_title,
      scene = list(
        xaxis = list(title = "VOT"),
        yaxis = list(title = "Closure Duration"),
        zaxis = list(title = "Closure Voicing")
      )
    )

  clustered_data <- cbind(
    subset_data,
    acoustic_data["cluster"]
  )

  confusion_matrix <- table(
    clustered_data$BaseStimuli,
    clustered_data$cluster
  )

  print(plot_3d)
  print(confusion_matrix)

  return(
    list(
      result = dbscan_result,
      clustered_data = clustered_data,
      confusion_matrix = confusion_matrix,
      plot = plot_3d
    )
  )
}


# --------------------------------------------------------------------------
# 1. INITIAL BILABIALS
# --------------------------------------------------------------------------

Initial_Bilabials <- subset(
  ProdData,
  Context == "Initial" &
    Place == "Bilabial"
)

initial_bilabial_scaled <- scale(
  Initial_Bilabials[
    ,
    c("VOT", "CD", "CV")
  ]
)

# Original epsilon exploration
inspect_eps(
  initial_bilabial_scaled,
  eps_candidates = c(
    0.3, 0.4, 0.5,
    0.6, 0.7, 0.8, 0.9
  ),
  minPts = 6
)

# Final value used in the original analysis: eps = 0.3
initial_bilabial_dbscan <- run_dbscan_analysis(
  subset_data = Initial_Bilabials,
  eps_value = 0.3,
  cluster_labels = c(
    "Noise",
    "Voiceless Unaspirated",
    "Voiceless Aspirated",
    "Voiced Unaspirated"
  ),
  plot_title =
    "3D Clustering of Bengali Initial Bilabial Production Data"
)


# --------------------------------------------------------------------------
# 2. MEDIAL BILABIALS
# --------------------------------------------------------------------------

Medial_Bilabials <- subset(
  ProdData,
  Context == "Medial" &
    Place == "Bilabial"
)

medial_bilabial_scaled <- scale(
  Medial_Bilabials[
    ,
    c("VOT", "CD", "CV")
  ]
)

# Original epsilon exploration
inspect_eps(
  medial_bilabial_scaled,
  eps_candidates = c(
    0.2, 0.3, 0.4,
    0.5, 0.6, 0.7, 0.8
  ),
  minPts = 6
)

# Final value used in the original analysis: eps = 0.5
medial_bilabial_dbscan <- run_dbscan_analysis(
  subset_data = Medial_Bilabials,
  eps_value = 0.5,
  cluster_labels = c(
    "Noise",
    "Voiceless Category",
    "Voiced Category",
    "Voiceless-Voiced Transition with Negative VOT"
  ),
  plot_title =
    "3D Clustering of Bengali Medial Bilabial Production Data"
)


# --------------------------------------------------------------------------
# 3. INITIAL VELARS
# --------------------------------------------------------------------------

Initial_Velars <- subset(
  ProdData,
  Context == "Initial" &
    Place == "Velar"
)

initial_velar_scaled <- scale(
  Initial_Velars[
    ,
    c("VOT", "CD", "CV")
  ]
)

# Original epsilon exploration
inspect_eps(
  initial_velar_scaled,
  eps_candidates = c(
    0.3, 0.4, 0.5,
    0.6, 0.7, 0.8, 0.9
  ),
  minPts = 6
)

# Final value used in the original analysis: eps = 0.5
initial_velar_dbscan <- run_dbscan_analysis(
  subset_data = Initial_Velars,
  eps_value = 0.5,
  cluster_labels = c(
    "Noise",
    "Voiceless Category"
  ),
  plot_title =
    "3D Clustering of Bengali Initial Velar Production Data"
)


# --------------------------------------------------------------------------
# 4. MEDIAL VELARS
# --------------------------------------------------------------------------

Medial_Velars <- subset(
  ProdData,
  Context == "Medial" &
    Place == "Velar"
)

medial_velar_scaled <- scale(
  Medial_Velars[
    ,
    c("VOT", "CD", "CV")
  ]
)

# Original epsilon exploration
inspect_eps(
  medial_velar_scaled,
  eps_candidates = c(
    0.2, 0.3, 0.4,
    0.5, 0.6, 0.7, 0.8
  ),
  minPts = 6
)

# Final value used in the original analysis: eps = 0.6
medial_velar_dbscan <- run_dbscan_analysis(
  subset_data = Medial_Velars,
  eps_value = 0.6,
  cluster_labels = c(
    "Noise",
    "Voiceless Category",
    "Voiced Category",
    "Longer Negative VOT"
  ),
  plot_title =
    "3D Clustering of Bengali Medial Velar Production Data"
)
