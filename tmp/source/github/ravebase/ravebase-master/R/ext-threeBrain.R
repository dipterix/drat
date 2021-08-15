#' Create 3D brain instance
#' @seealso \code{\link[threeBrain]{freesurfer_brain2}}
#' @param subject character or \code{RAVESubject} instance
#' @param surfaces surface types, usually stored as \code{'*h.xxx'} in
#' 'FreeSurfer' folder or 'SUMA' folder
#' @param use_141 whether to use 'SUMA' standard 141 brain if exists
#' @param recache,clean_before_cache whether to re-calculate cache and
#' clean cache before redo cache
#' @param compute_template whether to compute for template brain, usually
#' use it when 141 brain is used
#' @param usetemplateifmissing if surface data is missing, should
#' template brain be used? this requires 'MNI305' coordinates
#' @return A \code{rave-brain} instance, see more at 3D viewer function
#' \code{\link[threeBrain]{freesurfer_brain2}}
#' @export
rave_brain2 <- function(subject, surfaces = 'pial', use_141 = TRUE,
                        recache = FALSE, clean_before_cache = FALSE,
                        compute_template = FALSE, usetemplateifmissing = FALSE){

  # if subject is NULL, use current loaded subject
  if( is.character( subject ) ){
    subject = as_rave_subject(subject, strict = FALSE)
  }

  # To find freesurfer directory, here are the paths to search
  # 1. rave_data/project/subject/rave/fs
  # 2. rave_data/project/subject/fs
  # 3. rave_data/project/subject/
  # 3. if options('rave.freesurfer_dir') is provided, then XXX/subject/

  fs_path = subject$freesurfer_path

  # load electrodes
  electrode_table = subject$meta_data('electrodes')

  if( is.na(fs_path) ){
    if( !usetemplateifmissing ){
      return(invisible())
    }

    brain = threeBrain::merge_brain()

    brain$set_electrodes(electrodes = electrode_table)

  }else{
    # import from freesurfer folder
    if(recache){
      if( clean_before_cache ){
        fs = list.files(file.path(fs_path, 'RAVE'), pattern = '\\.json$',
                        all.files = FALSE, recursive = FALSE, full.names = TRUE,
                        ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
        lapply(fs, unlink)
      }
      threeBrain::import_from_freesurfer(fs_path, subject_name = subject$subject_code)
    }
    brain = threeBrain::freesurfer_brain2(
      fs_subject_folder = fs_path, subject_name = subject$subject_code,
      surface_types = surfaces, use_141 = use_141)

    brain$set_electrodes(electrodes = electrode_table)

    if( compute_template ){
      tf = tempfile()
      new_table = brain$calculate_template_coordinates(save_to = tf)
      if( file.exists(tf) ){
        brain$electrodes$raw_table_path = NULL
        unlink(tf)
        # need to update meta
        save_meta(new_table, meta_type = 'electrodes',
                  project_name = subject$project_name,
                  subject_code = subject$subject_code)
      }
    }
  }


  brain
}
