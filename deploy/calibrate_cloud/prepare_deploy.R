# deploy/calibrate_cloud/prepare_deploy.R
# ─────────────────────────────────────────────────────────────────
# Copies the latest aggregate data from the main project into the
# deploy folder before pushing to shinyapps.io.
#
# Usage (from project root):
#   source("deploy/calibrate_cloud/prepare_deploy.R")
#   rsconnect::deployApp("deploy/calibrate_cloud")
# ─────────────────────────────────────────────────────────────────

project_root <- here::here()
deploy_dir   <- file.path(project_root, "deploy", "calibrate_cloud", "data")

dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)

file.copy(
  file.path(project_root, "output", "presentation_data_marovoay.xlsx"),
  file.path(deploy_dir, "presentation_data_marovoay.xlsx"),
  overwrite = TRUE
)
file.copy(
  file.path(project_root, "output", "presentation_data_alaotra.xlsx"),
  file.path(deploy_dir, "presentation_data_alaotra.xlsx"),
  overwrite = TRUE
)
file.copy(
  file.path(project_root, "data", "presentation_sizes.yml"),
  file.path(deploy_dir, "presentation_sizes.yml"),
  overwrite = TRUE
)

message("Deploy data updated in: ", deploy_dir)
