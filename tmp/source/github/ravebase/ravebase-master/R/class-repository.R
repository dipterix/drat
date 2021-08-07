
#' Create or get \code{\link{RAVERepository}} instance
#' @description If repository has been loaded with the same signature, the
#' loaded one will be returned, otherwise generate a new repository
#' @param subject character or \code{\link{RAVESubject}} instance
#' @param reference reference name
#' @param epoch epoch name
#' @param before_onset seconds to be loaded before trial onset
#' @param after_onset seconds to be loaded after trial onset
#' @return \code{\link{RAVERepository}} instance
#' @seealso \code{\link{RAVERepository}}
#'
#' @examples
#' \dontrun{
#'
#' # Create repository for demo subject 'demo/DemoSubject',
#' # reference is reference_default.csv
#' # epoch is reference_auditory_onset.csv
#' # Time span is -1 ~ 2 (total 3 seconds, 0s is auditory onset)
#' repo <-
#'   loaded_rave_repository(
#'     subject = 'demo/DemoSubject',
#'     reference = 'default',
#'     epoch = 'auditory_onset',
#'     before_onset = 1,
#'     after_onset = 2
#'   )
#'
#' # Epoch and get power data
#' power <- repo$epoch_power()
#'
#' power
#'
#' # get repository in other place instead of creating a new one
#' repo2 <-
#'   loaded_rave_repository(
#'     subject = 'demo/DemoSubject',
#'     reference = 'default',
#'     epoch = 'auditory_onset',
#'     before_onset = 1,
#'     after_onset = 2
#'   )
#'
#' identical(repo2, repo)
#'
#' # remove disk data
#' repo$clear_cache()
#'
#' # release RAM
#' repo$clear_memory()
#'
#' }
#'
#' @export
loaded_rave_repository <- function(subject, reference, epoch, before_onset, after_onset){
  repo = RAVERepository$new(subject, reference, epoch, before_onset, after_onset)
  sig = repo$signature

  rave_repos <- get('rave_repos')

  # calculate digest and save to rave_repos
  rave_repos[[sig]] %?<-% repo

  rave_repos[[sig]]
}

#' Definition for data repository class
#' @description Do not create instance directly, use
#' \code{\link{loaded_rave_repository}} instead.
#' @export
RAVERepository <- R6::R6Class(
  classname = 'RAVERepository',
  cloneable = FALSE,
  private = list(
    default_electrodes = NULL
  ),
  public = list(

    #' @field subject \code{\link{RAVESubject}} instance
    subject = NULL,

    #' @field electrodes electrodes loaded
    electrodes = NULL,

    #' @field ignored_electrodes electrodes ignored
    ignored_electrodes = NULL,

    #' @field reference_table reference table
    reference_table = NULL,

    #' @field time_range seconds before and after trial onset
    time_range = c(NA_real_, NA_real_),

    #' @field reference_name name of reference table
    reference_name = character(0),

    #' @field epoch \code{\link{RAVEEpoch}} instance
    epoch = NULL,

    #' @description constructor
    #' @param subject character or \code{\link{RAVESubject}} instance
    #' @param reference reference name
    #' @param epoch epoch name
    #' @param before_onset seconds to be loaded before trial onset
    #' @param after_onset seconds to be loaded after trial onset
    initialize = function(subject, reference, epoch, before_onset, after_onset){
      stopifnot2(before_onset >= 0, after_onset >= 0, msg = 'Both before_onset and after_onset must be non-negative')
      self$time_range = c(-before_onset, after_onset)

      subject = as_rave_subject(subject)
      self$subject = subject

      epoch = RAVEEpoch$new(subject = subject, name = epoch)
      self$epoch = epoch

      stopifnot2(isTRUE(reference %in% subject$reference_names), msg = sprintf('Reference %s is missing', reference))
      self$reference_name = reference
      reference_table = subject$meta_data(meta_type = 'reference', meta_name = reference)
      self$reference_table = reference_table
      self$ignored_electrodes = reference_table$Electrode[reference_table$Reference == '']

      electrodes = subject$electrodes
      electrode_types = subject$electrode_types

      private$default_electrodes <- electrodes[! electrodes %in% self$ignored_electrodes]

      generators = sapply(unique(electrode_types), function(ty){
        get0(sprintf('%s_electrode', ty), ifnotfound = Continuous_Electrode)
      }, simplify = FALSE, USE.NAMES = TRUE)

      li = lapply(seq_along(electrodes), function(ii){
        e = electrodes[[ii]]
        re = reference_table[reference_table$Electrode == e, ]
        stopifnot2(nrow(re) > 0, msg = sprintf('Reference table contains no electrode %d', e))
        re$Electrode_Type = electrode_types[[ii]]
        re
      })

      reference_electrodes = dipsaus::fastmap2()
      electrodes = dipsaus::fastmap2()

      lapply(li, function(row){
        etype = row$Electrode_Type
        e = generators[[etype]]$new(subject = subject, number = row$Electrode, is_reference = FALSE)

        reference_electrodes$etype %?<-% dipsaus::fastmap2()
        reference_electrodes$etype[[row$Reference]] %?<-% generators[[etype]]$new(
          subject = subject, number = row$Reference, is_reference = TRUE)

        e$set_reference(reference_electrodes$etype[[row$Reference]])
        e$.set_cachename(sprintf('%s-%s', etype, reference))
        e$set_epoch(epoch)

        electrodes[[as.character(row$Electrode)]] = e
        NULL
      })

      self$electrodes = electrodes

    },

    #' @description load 3D brain instance
    #' @param surfaces surface types such as \code{'pial'}, \code{'white'},
    #' \code{'smoothwm'}, \code{'sphere'}, etc.
    #' @param use_141 whether to use 'SUMA' 141 standard brain if possible
    #' @param recache whether to force re-calculate cache data
    #' @param clean_before_cache whether to clear data before caching
    #' @param compute_template whether to compute template vertices
    #' @param usetemplateifmissing whether to use template brain if surfaces
    #' are missing, default is false
    #' @return \code{threeBrain} brain instance
    #' @seealso \code{\link{rave_brain2}}, \code{\link[threeBrain]{freesurfer_brain2}}
    load_brain = function(surfaces = 'pial', use_141 = TRUE,
                          recache = FALSE, clean_before_cache = FALSE,
                          compute_template = FALSE, usetemplateifmissing = FALSE){
      call = match.call()
      call[[1]] = quote(rave_brain2)
      call[['subject']] = quote(self$subject)
      eval(call)
    },

    #' @description validate whether cached data is valid and consistent, i.e.
    #' the cached data is reliable, otherwise the cached data is no longer
    #' useful.
    #' @param dtype data type to check, choices are 'power', 'phase', 'voltage'
    #' @return If there is no valid electrode, then returns \code{NULL};
    #' otherwise returns a list with the following items:
    #' \describe{
    #' \item{path}{path where the cached data should be stored}
    #' \item{exist}{whether path exists as a directory}
    #' \item{consistent}{whether cached data is consistent}
    #' \item{dim}{dimension of the array}
    #' \item{dimnames}{dimension names of the array}
    #' }
    .validate_cache = function(dtype = c('power', 'phase', 'voltage')){
      dtype <- match.arg(dtype)
      self$epoch$update_table()
      all_electrodes <- self$subject$electrodes
      ignored_electrodes = self$ignored_electrodes
      electrodes <- as.integer(all_electrodes[!all_electrodes %in% ignored_electrodes])
      n_elecs <- length(electrodes)
      if(!n_elecs){
        # nothing to load
        rave_error('No electrodes to be loaded')
        return(NULL)
      }

      before_onset = -self$time_range[[1]]
      after_onset = self$time_range[[2]]

      # Determine cache path
      tmp <- sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      obj <- self$electrodes[[electrodes[[1]]]]
      fst_cache_dir <- file.path(obj$cache_path, dtype, tmp)

      # array dimension info
      dim_info <-
        obj$.dimension(before_onset = before_onset,
                       after_onset = after_onset,
                       dtype = dtype)

      exist <- FALSE
      consistent <- FALSE
      if(dir.exists(fst_cache_dir)){
        exist <- TRUE
        tryCatch({
          # check signature
          sig_file <- file.path(fst_cache_dir, 'rave.signature')
          sig <- raveio::load_yaml(sig_file)
          if(
            sig$signature == self$signature &&
            sig$before_onset == before_onset &&
            sig$after_onset == after_onset
          ){
            # cache is consistent
            consistent <- TRUE
          }
        }, error = do_nothing, warning = do_nothing)
      }

      list(
        path = fst_cache_dir,
        exist = exist,
        consistent = consistent,
        dim = dim_info$dim,
        dimnames = dim_info$dimnames
      )

    },

    #' @description internally used to save signature to cached arrays
    #' @param path path to the cached array
    .signature_yaml = function(path){
      sig_file <- file.path(path, 'rave.signature')
      sig <- list(
        signature = self$signature,
        before_onset = -self$time_range[[1]],
        after_onset = self$time_range[[2]]
      )
      raveio::save_yaml(sig, sig_file)
      normalizePath(sig_file)
    },

    #' @description epoch power, phase, or voltage data
    #' @param electrodes electrodes to epoch
    #' @param dtype data type, choices are 'power', 'phase', and 'voltage'
    epoch_continuous_signals = function(electrodes = NULL, dtype = c('power', 'phase', 'voltage')){
      dtype = match.arg(dtype)
      valid <- self$.validate_cache(dtype = dtype)

      # valid is NULL - no electrode valid
      if(is.null(valid)){ return(NULL) }
      all_electrodes <- self$subject$electrodes
      if(!length(electrodes)){
        electrodes = self$preload_electrodes
      }
      ignored_electrodes = self$ignored_electrodes
      electrodes = as.integer(electrodes[(!electrodes %in% ignored_electrodes) &
                                           (electrodes %in% all_electrodes)])

      n_elecs <- length(electrodes)
      if(!n_elecs){
        # nothing to load
        rave_error('No electrodes to be loaded')
        return(NULL)
      }

      before_onset = -self$time_range[[1]]
      after_onset = self$time_range[[2]]


      if(valid$exist && !valid$consistent){
        # invalid cache, remove it
        fs <- list.files(valid$path, include.dirs = FALSE, full.names = TRUE)
        unlink(fs, force = TRUE)
      }

      rave_cache_path(check = TRUE)

      arr <- lazyarray::lazyarray(valid$path, storage_format = 'double',
                                  dim = valid$dim, dimnames = valid$dimnames,
                                  quiet = TRUE, read_only = TRUE)
      self$.signature_yaml(valid$path)

      # check electrodes that are cached
      is_cached <- vapply(electrodes, function(e){
        self$electrodes[[as.character(e)]]$is_cached(
          before_onset = before_onset,
          after_onset = after_onset,
          dtype = dtype
        )
      }, FUN.VALUE = FALSE)
      electrodes <- electrodes[!is_cached]

      # Needs to save self to file, reduce memory before saving
      self$clear_memory()
      method <- sprintf('epoch_%s', dtype)
      async_work(electrodes, function(e){
        self$electrodes[[as.character(e)]][[method]](
          before_onset = before_onset,
          after_onset = after_onset
        )
        NULL
      }, .globals = list(
        self = self,
        before_onset = before_onset,
        after_onset = after_onset,
        method = method
      ), .wait = TRUE, .chunk_size = 4,
      .name = sprintf('Loading %s (%s)', self$subject$subject_id, dtype))

      # dipsaus::make_forked_clusters(rave_options('max_worker'))
      # re <- dipsaus::lapply_async2(electrodes, function(e){
      #   self$electrodes[[as.character(e)]]$epoch_power(
      #     before_onset = before_onset,
      #     after_onset = after_onset
      #   )
      #   NULL
      # }, callback = function(e){
      #   sprintf('Loading electrode %s', e)
      # }, future.chunk.size = 1)
      # lazyarray::set_lazy_threads(rave_options('max_worker'))
      arr
    },

    #' @description call \code{clear_cache} for each electrodes loaded
    #' @param dtypes data types to clear
    clear_cache = function(dtypes = c('power', 'phase', 'voltage')){
      # remove temparary folder
      before_onset = -self$time_range[[1]]
      after_onset = self$time_range[[2]]
      tmp = sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)

      nms = names(self$electrodes)
      for(dtype in dtypes){
        for(el in nms){
          d = file.path(self$electrodes[[el]]$cache_path, dtype, tmp)
          if( dir.exists(d) ){ unlink(d, recursive = TRUE )}
        }
      }

    },

    #' @description call \code{clear_memory} for each electrodes loaded
    clear_memory = function(){
      nms = names(self$electrodes)
      for(el in nms){
        self$electrodes[[el]]$clear_memory()
      }
    },


    #' @description get meta data
    #' @param name meta type name, choices are \code{'electrode'},
    #' \code{'frequencies'}, \code{'time_points'}, \code{'trials'}
    #' @return Data frames of corresponding meta data
    get_meta = function(name = c('electrodes', 'frequencies', 'time_points', 'trials')){
      name <- match.arg(name)
      switch(
        name,
        'electrodes' = {
          elec <- self$subject$meta_data(meta_type = 'electrodes')
          refs <- self$reference_table
          merge(elec, refs, all.x = TRUE, by = 'Electrode')
        },
        'frequencies' = {
          self$subject$meta_data(meta_type = 'frequencies')
        },
        'time_points' = {
          self$subject$meta_data(meta_type = 'time_points')
        },
        'trials' = {
          self$epoch$table
        }
      )
    },

    #' @description get all valid electrode numbers for current subject
    #' @param electrodes integers to filter, if missing then default to all
    #' possible electrodes
    #' @return Valid electrode numbers
    get_valid_electrodes = function(electrodes){
      if(missing(electrodes)){
        electrodes <- self$subject$electrodes
      }
      electrodes[
        !electrodes %in% self$ignored_electrodes &
        electrodes %in% self$subject$electrodes
      ]
    },

    #' @description get electrode numbers to be loaded
    get_loaded_electrodes = function(){
      self$preload_electrodes
    },

    #' @description epoch power data of pre-specified electrodes
    #' @param .old whether use 'RAVE' 1.0 format
    #' @param ... ignored
    #' @return if \code{.old} is true, return \code{\link[raveio]{ECoGTensor}}
    #' instance, otherwise return \code{\link[lazyarray]{lazyarray}}
    get_power = function(.old = FALSE, ...){
      electrodes <- self$preload_electrodes
      re <- self$epoch_continuous_signals(electrodes = electrodes, dtype = 'power')
      if(.old){
        re <- raveio::lazyarray_to_tensor(re, drop_partition = TRUE)
        re <- re$subset(Electrode ~ Electrode %in% electrodes, drop = FALSE, data_only = FALSE)
      }
      re
    },

    #' @description epoch phase data of pre-specified electrodes
    #' @param .old whether use 'RAVE' 1.0 format
    #' @param ... ignored
    #' @return if \code{.old} is true, return \code{\link[raveio]{ECoGTensor}}
    #' instance, otherwise return \code{\link[lazyarray]{lazyarray}}
    get_phase = function(.old = FALSE, ...){
      electrodes <- self$preload_electrodes
      re <- self$epoch_continuous_signals(electrodes = electrodes, dtype = 'phase')
      if(.old){
        re <- raveio::lazyarray_to_tensor(re, drop_partition = TRUE)
        re <- re$subset(Electrode ~ Electrode %in% electrodes, drop = FALSE, data_only = FALSE)
      }
      re
    },

    #' @description epoch voltage data of pre-specified electrodes
    #' @param .old whether use 'RAVE' 1.0 format
    #' @param ... ignored
    #' @return if \code{.old} is true, return \code{\link[raveio]{Tensor}}
    #' instance, otherwise return \code{\link[lazyarray]{lazyarray}}
    get_voltage = function(.old = FALSE, ...){
      electrodes <- self$preload_electrodes
      re <- self$epoch_continuous_signals(electrodes = electrodes, dtype = 'voltage')
      if(.old){
        re <- raveio::lazyarray_to_tensor(re, drop_partition = TRUE)
        re <- re$subset(Electrode ~ Electrode %in% electrodes, drop = FALSE, data_only = FALSE)
      }
      re
    },

    #' @description get sampling frequency
    #' @param type electrode signal type, choices are \code{'ECoG'},
    #' \code{'LFP'}, \code{'Spike'}, \code{'EEG'}
    #' @param time_freq whether should return sample rates after time-frequency
    #' decomposition (e.g. down-sampled rate after wavelet)
    #' @return if \code{time_freq} is true, return power or phase sample rate,
    #' otherwise return voltage signal sample rate. If there is no such
    #' electrode signal type, for example, \code{type='Spike'} but no spike
    #' signals, then return \code{NA}
    get_sample_rate = function(type = c('ECoG', 'LFP', 'Spike', 'EEG'), time_freq = FALSE){
      type <- match.arg(type)
      if(time_freq){
        return(self$subject$power_sample_rate)
      }
      # voltage data
      e_type <- self$subject$electrode_types
      srates <- self$subject$raw_sample_rates
      if(type %in% c('ECoG', 'LFP')){
        srates <- srates[e_type %in% c('ECoG', 'LFP')]
      } else {
        srates <- srates[e_type %in% type]
      }
      if(length(srates)){
        return(srates[[1]])
      } else {
        return(NA)
      }
    },

    #' @description get information when creating the repository
    #' @return A list containing electrode to analyze, epoch and reference table
    #' names, time range of the analysis, time index for power and phase,
    #' wavelet frequencies and trial condition types.
    get_preload_info = function(){
      re <- dipsaus::fastmap2()
      re$electrodes <- self$preload_electrodes

      re$epoch_name <- self$epoch$name
      re$reference_name <- self$reference_name

      before_onset <- -self$time_range[1]
      after_onset <- self$time_range[2]
      srate_power <- self$subject$power_sample_rate
      re$time_points_power <- seq.int(- ceiling(before_onset * srate_power), ceiling(after_onset * srate_power))
      re$time_points_phase <- re$time_points_power

      re$frequencies <- self$subject$meta_data('frequencies')$Frequency
      re$condition <- unique(self$epoch$table$Condition)

      re
    }



  ),
  active = list(

    #' @field signature unique signature for repository; if subject ID,
    #' reference table, epoch table, and time range are the same, signatures
    #' will be the same
    signature = function(){
      digest::digest(list(
        subject = self$subject$subject_id,
        reference = self$reference_table,
        epoch = self$epoch$table,
        time_range = self$time_range,
        wavelet = self$subject$preprocess_settings$wavelet_params
      ))
    },


    #' @field preload_electrodes get or set default electrodes to be loaded,
    #' mainly used internally by \code{link{rave_load}}.
    preload_electrodes = function(v){
      if(!missing(v)){
        v <- v[v %in% self$subject$electrodes]
        v <- v[!v %in% self$ignored_electrodes]
        private$default_electrodes <- v
      }
      private$default_electrodes
    },

    #' @field project \code{\link{RAVEProject}} instance
    project = function(){
      self$subject$project
    }
  )
)
