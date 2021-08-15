#' @title Load or attach 'RAVE' subject
#' @name rave_load
#' @description Load or attach 'RAVE' a subject to current analysis context.
#' The goal is to allow analysis and visualization to be focused on one that
#' subject.
#' @param x either \code{\link{RAVERepository}}, \code{\link{RAVESubject}}
#' instance or character, or \code{NULL}. For character, it's identical to
#' \code{subject}; for \code{NULL}, current attached repository will be
#' detached.
#' @param subject character, the format is \code{project/subject}, for example
#' \code{"demo/DemoSubject"}
#' @param epoch character, name of epoch, located in subject's 'meta' folder,
#' with file name \code{"epoch_<name>.csv"}
#' @param reference character, name of reference scheme, located in subject's
#' 'meta' folder, with file name \code{"reference_<name>.csv"}
#' @param before_onset seconds before trial onset
#' @param after_onset seconds after trial onset
#' @param electrodes electrodes to load, if missing then default to all
#' possible electrodes
#' @param attach whether to attach loaded repository for analysis
#' @param test whether return \code{NULL} instead of raising errors when
#' no subject is attached
#' @param ... passed to other methods
#' @return \code{rave_load} and \code{rave_attach} returns loaded repository,
#' an instance of \code{\link{RAVERepository}} class. \code{attached_repo}
#' returns currently loaded repository. If no repository loaded then it raises
#' errors.
#' @details \code{rave_attached} is a list providing various of methods that
#' handles attached subjects. It also allows 'RAVE' modules designed for
#' single-subject analysis to get information of what's to be analyzed.
#'
#' \code{epoch} and \code{reference} are located in subject's meta folder. The
#' folder path is \code{"<RAVE data_path>/project/subject/rave/meta/"}. Epoch
#' files start with \code{"epoch_"}, and its name is decided by the characters
#' following the prefix. For example, an epoch named "auditory" will have file
#' name \code{"epoch_auditory.csv"}. This rule applies to reference files,
#' except that the prefix is \code{"reference_"}
#'
#' \code{before_onset} and \code{after_onset} affects the amount of data
#' to be loaded. For example, \code{before_onset=1} and \code{after_onset=2}
#' will result in total 3 seconds of data to be loaded: 1 second before trial
#' onset and 2 after onset. The reason to load data before onset is to leave
#' certain time range for baseline. If the trial lasts for 2 seconds, loading
#' 3 seconds of data allows around 1 second to be counted into baseline.
#' The trial onset time for each trial is defined at column \code{"Time"} in
#' the epoch file.
#'
#' If \code{attach=TRUE}, then \code{rave_load} is equivalent to
#' \code{rave_attach}.
#'
#'
NULL

#' @rdname rave_load
#' @export
rave_load <- function(subject, epoch, reference, before_onset, after_onset, electrodes, attach = FALSE){
  rave_repos <- get('rave_repos')

  if(missing(subject)){
    rave_repos[['..current_repo']] <- list()
  }
  repo <-
    loaded_rave_repository(
      subject = subject,
      reference = reference,
      epoch = epoch,
      before_onset = before_onset,
      after_onset = after_onset
    )
  if(missing(electrodes)){
    electrodes <- repo$subject$electrodes
  }
  repo$preload_electrodes <- electrodes

  if(attach){
    rave_repos[['..current_repo']] <- list(
      signature = repo$signature,
      preload_electrodes = repo$preload_electrodes,
      before_onset = before_onset,
      after_onset = after_onset
    )
  }

  repo
}

#' @rdname rave_load
#' @export
rave_attach <- function(x, ...){
  if(is.null(x)){
    rave_repos <- get('rave_repos')
    rave_repos[['..current_repo']] <- list()
  } else {
    UseMethod('rave_attach')
  }
}

#' @rdname rave_load
#' @export
rave_attach.RAVERepository <- function(x, ...){
  rave_repos <- get('rave_repos')

  if(attach){
    rave_repos[['..current_repo']] <- list(
      signature = x$signature,
      preload_electrodes = x$preload_electrodes,
      before_onset = -x$time_range[1],
      after_onset = x$time_range[2]
    )
  }
  invisible(x)
}

#' @rdname rave_load
#' @export
rave_attach.character <- function(x, epoch, reference, before_onset, after_onset, electrodes, ...){
  rave_load(x, epoch, reference, before_onset, after_onset, electrodes, attach = TRUE)
}

#' @rdname rave_load
#' @export
rave_attach.RAVESubject <- function(x, epoch, reference, before_onset, after_onset, electrodes, ...){
  rave_load(x, epoch, reference, before_onset, after_onset, electrodes, attach = TRUE)
}



#' @rdname rave_load
#' @export
attached_repo <- function(test = FALSE){
  # rave_repos[['..current_repo']] <- list(
  #   signature = repo$signature,
  #   preload_electrodes = 13:20
  # )
  rave_repos <- get('rave_repos')
  info <- rave_repos$..current_repo
  if(is.list(info) && length(info$signature) && info$signature %in% names(rave_repos)){
    return(rave_repos[[info$signature]])
  }
  if(!test){
    rave_fatal('Cannot find any subject attached. Please run `rave_attach(...)` or `rave_load(..., attach=TRUE)` to attach RAVE subject first.')
  }
  return()
}


#' @rdname rave_load
#' @export
rave_attached <- structure(list(
  any_attached = function(){
    inherits(attached_repo(test = TRUE), 'RAVERepository')
  },
  epoch_names = function(){
    attached_repo()$subject$epoch_names
  },
  reference_names = function(){
    attached_repo()$subject$reference_names
  },
  get_meta = function(name){
    repo <- attached_repo()
    repo$get_meta(name)
  },
  get_valid_electrodes = function(electrodes){
    repo <- attached_repo()
    repo$get_valid_electrodes(electrodes)
  },
  get_loaded_electrodes = function(){
    repo <- attached_repo()
    repo$get_loaded_electrodes()
  },
  get_power = function(use_cache = TRUE, .old = FALSE, ...){
    if('referenced' %in% names(list(...))){
      stop('$get_power(referenced=) is no longer supported.')
    }

    repo <- attached_repo()
    repo$get_power(.old = .old, ...)
  },
  get_phase = function(use_cache = TRUE, .old = FALSE, ...){

    if('referenced' %in% names(list(...))){
      stop('$get_phase(referenced=) is no longer supported.')
    }

    repo <- attached_repo()
    repo$get_phase(.old = .old, ...)
  },
  get_voltage = function(use_cache = TRUE, .old = FALSE, ...){

    if('referenced' %in% names(list(...))){
      stop('$get_voltage(referenced=) is no longer supported.')
    }

    repo <- attached_repo()
    repo$get_voltage(.old = .old, ...)
  },
  get_sample_rate = function(type = c('ECoG', 'LFP', 'Spike', 'EEG'), time_freq = FALSE){
    repo <- attached_repo()
    type <- match.arg(type)
    repo$get_sample_rate(type = type, time_freq = time_freq)
  },
  get_preload_info = function(){
    repo <- attached_repo()
    repo$get_preload_info()
  },
  get_repository = function(){
    attached_repo()
  },
  get_subject = function(compare = NULL){
    if( inherits(compare, 'RAVESubject') ){
      sub_id <- compare$subject_id
    } else {
      sub_id <- compare
    }
    is_subject <- inherits(sub_id, 'character') && length(sub_id) == 1
    repo <- attached_repo(test = TRUE)

    if(inherits(repo, 'RAVERepository')){
      # return current attached subject, but compare the ID
      return(list(
        subject = repo$subject,
        identical = isTRUE(repo$subject$subject_id == sub_id),
        attached = TRUE
      ))
    } else if(is_subject){
      return(list(
        subject = as_rave_subject(compare),
        identical = FALSE,
        attached = FALSE
      ))
    } else {
      return(list(
        subject = NULL,
        identical = FALSE,
        attached = FALSE
      ))
    }

  },
  get_project = function(compare = NULL){
    if(inherits(compare, 'RAVEProject')){
      project_name <- compare$name
    } else {
      project_name <- compare
    }
    # if(inherits(project_name, 'character') && length(project_name) == 1)
    repo <- attached_repo(test = TRUE)
    if(inherits(repo, 'RAVERepository')){
      # has project
      return(list(
        project = repo$project,
        identical = isTRUE(repo$project$name == project_name),
        attached = TRUE
      ))
    } else if (inherits(project_name, 'character') && length(project_name) == 1) {
      return(list(
        project = as_rave_project(compare),
        identical = FALSE,
        attached = FALSE
      ))
    } else {
      return(list(
        project = NULL,
        identical = FALSE,
        attached = FALSE
      ))
    }
  }
), class = c('ravebase_rave_attached', 'ravebase_readonly'))

#' @export
`$<-.ravebase_readonly` <- function(x, name, value){
  stop('Cannot assign values to read-only object')
}

#' @export
`[[<-.ravebase_readonly` <- `$<-.ravebase_readonly`

#' @export
`[.ravebase_readonly` <- function(x, i, ...){
  stop('Cannot subset read-only object')
}

#' @export
print.ravebase_rave_attached <- function(x, ...){
  cat('<rave_attached> - query data from attached RAVE subject\n')
  cat('  x$any_attached()           - return TRUE/FALSE whether any subject attached\n')
  cat('  x$get_power()              - get epoched & referenced power data\n')
  cat('  x$get_phase()              - get epoched & referenced phase angles\n')
  cat('  x$get_voltage()            - get epoched & referenced trace data\n')
  cat('  x$get_meta()               - get meta table such as `electrodes`, `trials`, `frequencies` etc.\n')
  cat('  x$get_sample_rate()        - get sampling frequency\n')
  cat('  x$get_valid_electrodes()   - get valid electrode numbers\n')
  cat('  x$get_loaded_electrodes()  - get default electrodes to load\n')
  cat('  x$get_preload_info()       - get loader information\n')
  cat('  x$get_repository()         - get current repository object (same as `attached_repo()`)\n')
  cat('  x$get_subject()            - get or compare current loaded subject\n')
  cat('  x$get_project()            - get or compare current loaded project\n')
  cat('  x$epoch_names()            - return all possible epoch names for the attached subject\n')
  cat('  x$reference_names()        - return all possible reference names for the attached subject\n')
  invisible(x)
}

