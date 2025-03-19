#' Initialize a soft calibration workflow
#'
#' `initialize_softcal()` initializes a template soft calibration project. The
#' project provides a template workflow in the folder '/workflow'. The folders
#' '/observation' and '/simulation' are generated for further use in the
#' simulation workflow. An R project is initialized and loaded in the new folder
#' structure.
#'
#' @param project_name Name of the soft calibration project.
#' @param path Path where the soft calibration project should be initialized.
#' @param model_path Path to the SWAT+ model.
#' @param start_new_session Open R project in new or current R session.
#'
#' @return Opens a template R project for SWAT+ soft calibration.
#' @export
#'
#' @examples
#' \dontrun{
#' initialize_softcal('softcal_test', '~/', 'Path:/to/txt_inout')
#' }
#'
initialize_softcal <- function(project_name, path, model_path,
                               start_new_session = FALSE) {
  # Project path of the new soft calibration workflow
  project_path <- paste0(path, '/', project_name)

  # Check if a folder with the same name already exists.
  if(dir.exists(project_path)) {
    stop("Folder '",  project_path, "' already exists!")
  }

  # Check if model_path is a SWAT+ project.
  if(!file.exists(paste0(model_path, '/object.cnt'))) {
    stop("Folder '",  project_path, "' already exists!")
  }

  # Create folder structure for hard calibration project.
  dir.create(project_path)
  dir.create(paste0(project_path, '/observation'))
  dir.create(paste0(project_path, '/simulation'))
  dir.create(paste0(project_path, '/workflow'))
  dir.create(paste0(project_path, '/backup'))

  # Get path of template workflow R scripts and copy all files into project.
  wf_path <- paste0(system.file(package = "SWATtunR"), '/extdata/wf_softcal')

  file.copy(list.files(wf_path, full.names = TRUE),
            paste0(project_path, '/workflow'))

  file.copy(paste0(system.file(package = "SWATtunR"), '/extdata/crop2.csv'),
            paste0(project_path, '/observation/crop_yields.csv'))

  # Add model_path to crop calibration script and copy into project.
  f01 <- readLines(paste0(wf_path, '/01_crop_calibration.R'))
  line_id <- grepl("model_path <- ''", f01)
  f01[line_id] <- paste0("model_path <- '", model_path, "'")
  writeLines(f01, paste0(project_path, '/workflow/01_crop_calibration.R'))

  # Add model_path to wyr calibration script and copy into project.
  f02 <- readLines(paste0(wf_path, '/02_wateryield_calibration.R'))
  line_id <- grepl("model_path <- ''", f02)
  f02[line_id] <- paste0("model_path <- '", model_path, "'")
  writeLines(f02, paste0(project_path, '/workflow/02_wateryield_calibration.R'))

  # Initialize and load R project
  rstudioapi::initializeProject(path = project_path)
  rstudioapi::openProject(path = project_path, newSession = start_new_session)
}


#' Initialize a hard calibration workflow
#'
#' `initialize_hardcal()` initializes a template hard calibration project. The
#' template project provides a template simulation workflow in the folder
#' '/workflow'. The folders '/observation' and '/simulation' are generated
#' for further use in the simulation workflow.
#' An R project is initialized and loaded in the new folder structure.
#'
#' @param project_name Name of the hard calibration project.
#' @param path Path where the hard calibration project should be initialized.
#' @param model_path Path to the SWAT+ model.
#' @param start_new_session Open R project in new or current R session.
#'
#' @return Opens a template R project for SWAT+ hard calibration.
#' @export
#'
#' @examples
#' \dontrun{
#' initialize_hardcal('hardcal_test', '~/', 'Path:/to/txt_inout')
#' }
#'
initialize_hardcal <- function(project_name, path, model_path,
                               start_new_session = FALSE) {
  # Project path of the new hard calibration workflow
  project_path <- paste0(path, '/', project_name)

  # Check if a folder with the same name already exists.
  if(dir.exists(project_path)) {
    stop("Folder '",  project_path, "' already exists!")
  }

  # Check if model_path is a SWAT+ project.
  if(!file.exists(paste0(model_path, '/object.cnt'))) {
    stop("Folder '",  project_path, "' already exists!")
  }

  # Create folder structure for hard calibration project.
  dir.create(project_path)
  dir.create(paste0(project_path, '/observation'))
  dir.create(paste0(project_path, '/simulation'))
  dir.create(paste0(project_path, '/workflow'))

  # Get path of template workflow R scripts and copy all files into project.
  wf_path <- paste0(system.file(package = "SWATtunR"), '/extdata/wf_hardcal')

  file.copy(list.files(wf_path, full.names = TRUE),
            paste0(project_path, '/workflow'))

  # Add model_path to run script and copy into project.
  f01 <- readLines(paste0(wf_path, '/01_define_parameter.R'))
  line_id <- grepl("model_path <- ''", f01)
  f01[line_id] <- paste0("model_path <- '", model_path, "'")
  writeLines(f01, paste0(project_path, '/workflow/01_define_parameter.R'))

  # Add model_path to run script and copy into project.
  f03 <- readLines(paste0(wf_path, '/03_run_swat.R'))
  line_id <- grepl("model_path <- ''", f03)
  f03[line_id] <- paste0("model_path <- '", model_path, "'")
  writeLines(f03, paste0(project_path, '/workflow/03_run_swat.R'))

  # Initialize and load R project
  rstudioapi::initializeProject(path = project_path)
  rstudioapi::openProject(path = project_path, newSession = start_new_session)
}
