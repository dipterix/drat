#' Definition for electrodes that collects continuous signals
#' @description Defines class of electrodes that record continuous signals.
#' These signals have meaningful power density and will be transformed for
#' time-frequency analysis. Unlike spike signals (discrete),
#' continuous signals usually represent signals fired from a group of neurons.
Continuous_Electrode <- R6::R6Class(
  classname = 'Continuous_Electrode',
  inherit = RAVEAbstarctElectrode,
  cloneable = FALSE,
  lock_class = TRUE,
  private = list(
    persisted_voltage_unref = NULL,
    persisted_power_unref = NULL,
    persisted_phase_unref = NULL,
    persisted_coef_ref = NULL,
    referenced_power_cache_file = character(0),
    referenced_phase_cache_file = character(0),
    referenced_voltage_cache_file = character(0)
  ),
  public = list(

    #' @field type type of signals collected by electrode
    type = 'Continuous',

    #' @description set reference for current electrode
    #' @param reference either \code{NULL} or \code{Continuous_Electrode} instance
    set_reference = function(reference){
      self$.set_reference(reference)
    },

    #' @description constructor
    #' @param subject,number,is_reference see constructor in
    #' \code{\link{RAVEAbstarctElectrode}}
    initialize = function(subject, number, is_reference = FALSE){
      super$initialize(subject, number, is_reference)
      if(is_reference){
        # this is a reference electrode
        self$is_reference <- TRUE
        ref_electrodes <- stringr::str_match(number, 'ref_([0-9\\-,\\ ]+)')[,2]

        # no reference value, 'noref'
        if(is.na(ref_electrodes)){
          ref_electrodes <- ''
        }

        e <- dipsaus::parse_svec(ref_electrodes)
        if(length(e) == 0){
          self$number <- 'noref'
        } else {
          if(length(e) == 1){
            self$number <- e
          } else {
            # check subject reference directory
            self$number <- sprintf('ref_%s.h5', ref_electrodes)
            if(!file.exists(file.path(self$subject$reference_path, self$number))){
              rave_warn("Reference file {self$number} is missing")
            }
          }
        }
      }

    },

    # data method

    #' @description load voltage data that is before referenced
    #' @param block experiment block
    #' @param persist whether to persist in the instance, default is false,
    #' however, if this function will be called multiple times, set it to true.
    #' @return voltage data before reference
    load_unreferenced_voltage = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_voltage_unref[[block]])){
          return(private$persisted_voltage_unref[[block]])
        }
      }


      if(is.numeric(self$number)){
        # load from data
        fst_path <- file.path(self$subject$cache_path, 'voltage', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path <- file.path(self$subject$cache_path, 'voltage', 'ref', block, self$fst_fname)
        }
        h5_path <- file.path(self$subject$data_path, 'voltage', self$h5_fname)
        h5_name <- sprintf('/raw/voltage/%s', block)

        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        # load from reference folder
        h5_path <- file.path(self$subject$reference_path, self$h5_fname)
        h5_name <- sprintf('/voltage/%s', block)
        re <- load_h5(h5_path, h5_name, read_only = TRUE, ram = FALSE)
      }

      if(persist){
        private$persisted_voltage_unref[[block]] <- re[]
        return(private$persisted_voltage_unref[[block]])
      }

      re

    },

    #' @description load power data that is before referenced
    #' @param block experiment block
    #' @param persist whether to persist in the instance, default is false,
    #' however, if this function will be called multiple times, set it to true.
    #' @return power data before reference
    load_unreferenced_power = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_power_unref[[block]])){
          return(private$persisted_power_unref[[block]])
        }
      }

      if(is.numeric(self$number)){
        # load from data
        fst_path <- file.path(self$subject$cache_path, 'power', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path <- file.path(self$subject$cache_path, 'power', 'ref', block, self$fst_fname)
        }
        h5_path <- file.path(self$subject$data_path, 'power', self$h5_fname)
        h5_name <- sprintf('/raw/power/%s', block)

        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        if(!persist){
          rave_fatal('load_unreferenced_power is not for reference_electrode unless persist is TRUE')
        }

        # load from reference folder
        h5_path <- file.path(self$subject$reference_path, self$h5_fname)
        h5_name <- sprintf('/wavelet/coef/%s', block)
        re <- load_h5(h5_path, h5_name, read_only = TRUE, ram = TRUE)

        re <- re[,,1, drop = FALSE] ^ 2
        dim(re) <- dim(re)[1:2]
        private$persisted_power_unref[[block]] <- re
        return(re)

      }

      if(persist){
        private$persisted_power_unref[[block]] <- re[]
        return(private$persisted_power_unref[[block]])
      }

      re

    },

    #' @description load phase data that is before referenced
    #' @param block experiment block
    #' @param persist whether to persist in the instance, default is false,
    #' however, if this function will be called multiple times, set it to true.
    #' @return phase data before reference
    load_unreferenced_phase = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_phase_unref[[block]])){
          return(private$persisted_phase_unref[[block]])
        }
      }

      if(is.numeric(self$number)){
        # load from data
        fst_path <- file.path(self$subject$cache_path, 'phase', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path <- file.path(self$subject$cache_path, 'phase', 'ref', block, self$fst_fname)
        }
        h5_path <- file.path(self$subject$data_path, 'phase', self$h5_fname)
        h5_name <- sprintf('/raw/phase/%s', block)

        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        if(!persist){
          rave_fatal('load_unreferenced_phase is not for reference_electrode unless persist is TRUE')
        }

        # load from reference folder
        h5_path <- file.path(self$subject$reference_path, self$h5_fname)
        h5_name <- sprintf('/wavelet/coef/%s', block)
        re <- load_h5(h5_path, h5_name, read_only = TRUE, ram = TRUE)

        re <- re[,,2, drop = FALSE]
        dim(re) <- dim(re)[1:2]
        private$persisted_phase_unref[[block]] <- re
        return(re)

      }

      if(persist){
        private$persisted_phase_unref[[block]] <- re[]
        return(private$persisted_phase_unref[[block]])
      }

      re

    },

    # reference

    #' @description reference power for given block
    #' @param block character, experiment block
    #' @return referenced power
    reference_power = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_power(block, persist = FALSE)[])
      }
      # check whether cached
      has_cached <- self$reference_equals_cached

      if( has_cached ){
        fst_path <- file.path(self$subject$cache_path, 'power', 'ref', block, self$fst_fname)
        h5_path <- file.path(self$subject$data_path, 'power', self$h5_fname)
        h5_name <- sprintf('/ref/power/%s', block)
        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        power <- self$load_unreferenced_power(block = block, persist = FALSE)
        phase <- self$load_unreferenced_phase(block = block, persist = FALSE)
        ref_power <- self$reference$load_unreferenced_power(block = block, persist = TRUE)
        ref_phase <- self$reference$load_unreferenced_phase(block = block, persist = TRUE)

        coef <- sqrt(power[]) * exp(phase[] * 1i) - sqrt(ref_power) * exp(ref_phase * 1i)
        re <- Mod(coef) ^ 2
      }

      re
    },

    #' @description reference phase for given block
    #' @param block character, experiment block
    #' @return referenced phase
    reference_phase = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_phase(block, persist = FALSE)[])
      }
      has_cached <- self$reference_equals_cached

      if( has_cached ){
        fst_path <- file.path(self$subject$cache_path, 'phase', 'ref', block, self$fst_fname)
        h5_path <- file.path(self$subject$data_path, 'phase', self$h5_fname)
        h5_name <- sprintf('/ref/phase/%s', block)
        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        power <- self$load_unreferenced_power(block = block, persist = FALSE)
        phase <- self$load_unreferenced_phase(block = block, persist = FALSE)
        ref_power <- self$reference$load_unreferenced_power(block = block, persist = TRUE)
        ref_phase <- self$reference$load_unreferenced_phase(block = block, persist = TRUE)

        coef <- sqrt(power[]) * exp(phase[] * 1i) - sqrt(ref_power) * exp(ref_phase * 1i)
        re <- Arg(coef)
      }

      re
    },

    #' @description reference voltage for given block
    #' @param block character, experiment block
    #' @return referenced voltage
    reference_voltage = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_voltage(block, persist = FALSE)[])
      }
      has_cached <- self$reference_equals_cached

      if( has_cached ){
        fst_path <- file.path(self$subject$cache_path, 'voltage', 'ref', block, self$fst_fname)
        h5_path <- file.path(self$subject$data_path, 'voltage', self$h5_fname)
        h5_name <- sprintf('/ref/voltage/%s', block)
        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        voltage <- self$load_unreferenced_voltage(block = block, persist = FALSE)
        ref_voltage <- self$reference$load_unreferenced_voltage(block = block, persist = TRUE)
        re <- voltage-ref_voltage
      }

      re
    },

    #' @description check whether power is cached in run-time set-ups
    #' @param before_onset seconds before trial onset
    #' @param after_onset seconds after trial onset
    #' @param dtype data type, choices are 'power', 'phase', and 'voltage'
    #' @return logical whether power has been cached in a \code{lazyarray}
    is_cached = function(before_onset, after_onset, dtype = c('power', 'phase', 'voltage')){
      dtype <- match.arg(dtype)
      # Repo signature will not be checked

      dinfo <- self$.dimension(before_onset = before_onset, after_onset = after_onset, dtype = dtype)

      tmp <- sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      fst_cache_dir <- file.path(self$cache_path, dtype, tmp)
      fst_cache_file <- file.path(fst_cache_dir, self$fst_fname)
      fst_cache_meta <- file.path(fst_cache_dir, 'lazyarray.meta')
      if(file.exists(fst_cache_file) && file.exists(fst_cache_meta)){
        arr <- lazyarray::load_lazyarray(fst_cache_dir, read_only = TRUE)
        return(all(dim(arr) - dinfo$dim == 0))
      } else {
        return(FALSE)
      }
    },

    #' @description internally used to determine data dimension and names
    #' @param before_onset seconds before trial onset
    #' @param after_onset seconds after trial onset
    #' @param dtype data type, choices are 'power', 'phase', and 'voltge'
    .dimension = function(before_onset, after_onset, dtype = c('power', 'phase', 'voltage')){
      dtype <- match.arg(dtype)
      stopifnot2(inherits(self$epoch, 'RAVEEpoch'), msg = 'You need to set electrode epoch first')
      preproc <- self$preprocess_info
      dnames <- list()
      dim <- rep(NA_integer_, 4L)
      dim[[1]] <- self$epoch$n_trials
      dnames$Trial <- self$epoch$trials

      if(dtype == 'voltage'){
        srate <- preproc$sample_rate
      } else {
        stopifnot2(isTRUE(preproc$has_wavelet), msg = sprintf('Electrode %s does not have power data', self$number))
        wave_info <- self$subject$preprocess_settings$wavelet_params
        srate <- wave_info$downsample_to
        dim[[2]] <- length(wave_info$frequencies)
        dnames$Frequency <- wave_info$frequencies
      }
      tidx <- seq.int(- ceiling(before_onset * srate), ceiling(after_onset * srate))

      dim[[3]] <- length(tidx)
      dnames$Time <- tidx / srate

      all_electrodes <- seq_len(max(self$subject$electrodes))
      dim[[4]] <- length(all_electrodes)
      dnames$Electrode <- all_electrodes
      dim <- dim[!is.na(dim)]
      list(
        dim = dim,
        dimnames = dnames,
        srate = srate,
        tidx = tidx
      )
    },

    #' @description perform epoch on power with epoch and reference
    #' @param before_onset seconds before trial onset
    #' @param after_onset seconds after trial onset
    #' @return An \code{lazyarray} object that can be subset like normal arrays
    epoch_power = function(before_onset, after_onset){
      dinfo <- self$.dimension(before_onset = before_onset, after_onset = after_onset, dtype = 'power')

      tbl = self$epoch$table
      trials = self$epoch$trials
      blk = unique(tbl$Block)

      power_srate <- dinfo$srate
      tidx = dinfo$tidx

      tmp <- sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      dir <- file.path(self$cache_path, 'power', tmp)

      array <- lazyarray::lazyarray(dir, storage_format = 'double',
                                  dim = dinfo$dim, dimnames = dinfo$dimnames,
                                  quiet = TRUE, read_only = FALSE)
      dim <- dim(array)
      electrode_idx <- self$number

      private$referenced_power_cache_file <- array$get_partition_fpath(electrode_idx, full_path = TRUE)
      if(file.exists(private$referenced_power_cache_file)){
        # temporarily cached
        rave_debug('Partition exists - electrode {self$number}')
        return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))
      }
      # else epoch
      results <- array(NA_real_, dinfo$dim[c(1,2,3)])

      for(b in blk){
        # print(b)
        ref_power <- self$reference_power(b)

        # find which trials in the block
        sel <- tbl$Block %in% b
        sub_tbl <- tbl[sel, ]
        slice_idx <- vapply(seq_len(nrow(sub_tbl)), function(ii){
          row <- sub_tbl[ii,]
          t_pos <- round(row$Time * power_srate)
          as.integer(t_pos + tidx)
        }, FUN.VALUE = tidx)
        # freq x time x trial
        ref_power <- ref_power[, as.vector(slice_idx)]
        dim(ref_power) <- c(dim[c(2, 3)], sum(sel))
        results[sel,,] <- aperm(ref_power, c(3,1,2))
      }

      # write to array
      array[,,,electrode_idx] <- results
      return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))

    },

    #' @description perform epoch on phase with epoch and reference
    #' @param before_onset seconds before trial onset
    #' @param after_onset seconds after trial onset
    #' @return An \code{lazyarray} object that can be subset like normal arrays
    epoch_phase = function(before_onset, after_onset){
      dinfo <- self$.dimension(before_onset = before_onset, after_onset = after_onset, dtype = 'phase')

      tbl = self$epoch$table
      trials = self$epoch$trials
      blk = unique(tbl$Block)

      phase_srate <- dinfo$srate
      tidx = dinfo$tidx

      tmp <- sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      dir <- file.path(self$cache_path, 'phase', tmp)

      array <- lazyarray::lazyarray(dir, storage_format = 'double',
                                    dim = dinfo$dim, dimnames = dinfo$dimnames,
                                    quiet = TRUE, read_only = FALSE)
      dim <- dim(array)
      electrode_idx <- self$number

      private$referenced_phase_cache_file <- array$get_partition_fpath(electrode_idx, full_path = TRUE)
      if(file.exists(private$referenced_phase_cache_file)){
        # temporarily cached
        rave_debug('Partition exists - electrode {self$number}')
        return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))
      }
      # else epoch
      results <- array(NA_real_, dinfo$dim[c(1,2,3)])

      for(b in blk){
        ref_phase <- self$reference_phase(b)

        # find which trials in the block
        sel <- tbl$Block %in% b
        sub_tbl <- tbl[sel, ]
        slice_idx <- vapply(seq_len(nrow(sub_tbl)), function(ii){
          row <- sub_tbl[ii,]
          t_pos <- round(row$Time * phase_srate)
          as.integer(t_pos + tidx)
        }, FUN.VALUE = tidx)
        # freq x time x trial
        ref_phase <- ref_phase[, as.vector(slice_idx)]
        dim(ref_phase) <- c(dim[c(2, 3)], sum(sel))
        results[sel,,] <- aperm(ref_phase, c(3,1,2))
      }

      # write to array
      array[,,,electrode_idx] <- results
      return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))

    },

    #' @description perform epoch on phase with epoch and reference
    #' @param before_onset seconds before trial onset
    #' @param after_onset seconds after trial onset
    #' @return An \code{lazyarray} object that can be subset like normal arrays
    epoch_voltage = function(before_onset, after_onset){
      dinfo <- self$.dimension(before_onset = before_onset, after_onset = after_onset, dtype = 'voltage')

      tbl = self$epoch$table
      trials = self$epoch$trials
      blk = unique(tbl$Block)

      voltage_srate <- dinfo$srate
      tidx = dinfo$tidx

      tmp <- sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      dir <- file.path(self$cache_path, 'voltage', tmp)

      array <- lazyarray::lazyarray(dir, storage_format = 'double',
                                    dim = dinfo$dim, dimnames = dinfo$dimnames,
                                    quiet = TRUE, read_only = FALSE)
      dim <- dim(array)
      electrode_idx <- self$number

      private$referenced_voltage_cache_file <- array$get_partition_fpath(electrode_idx, full_path = TRUE)
      if(file.exists(private$referenced_voltage_cache_file)){
        # temporarily cached
        rave_debug('Partition exists - electrode {self$number}')
        return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))
      }
      # else epoch
      results <- array(NA_real_, dinfo$dim[c(1,2)])

      for(b in blk){
        ref_voltage <- self$reference_voltage(b)

        # find which trials in the block
        sel <- tbl$Block %in% b
        sub_tbl <- tbl[sel, ]
        slice_idx <- vapply(seq_len(nrow(sub_tbl)), function(ii){
          row <- sub_tbl[ii,]
          t_pos <- round(row$Time * voltage_srate)
          as.integer(t_pos + tidx)
        }, FUN.VALUE = tidx)
        # freq x time x trial
        ref_voltage <- ref_voltage[as.vector(slice_idx)]
        dim(ref_voltage) <- c(dim[2], sum(sel))
        results[sel,] <- t(ref_voltage)
      }

      # write to array
      array[,,electrode_idx] <- results
      return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))

    },


    #' @description method to clear cache on hard drive
    #' @param ... ignored
    clear_cache = function(...){
      f <- private$referenced_power_cache_file
      if(length(f) && file.exists(f)){
        unlink(f)
      }
    },

    #' @description method to clear memory
    #' @param ... ignored
    clear_memory = function(...){
      private$persisted_voltage_unref <- NULL
      private$persisted_power_unref <- NULL
      private$persisted_phase_unref <- NULL
      private$persisted_coef_ref <- NULL
      if(inherits(self$reference, 'RAVEAbstarctElectrode')){
        self$reference$clear_memory()
      }
    }

  ),
  active = list(
    #' @field exists whether electrode exists in subject
    exists = function(){
      if(!self$is_reference || is.numeric(self$number)){
        super$exists
      } else if( isTRUE(self$number == 'noref') ) {
        return(TRUE)
      } else {
        file.exists(file.path(self$subject$reference_path, self$number))
      }
    },

    #' @field h5_fname 'HDF5' file name
    h5_fname = function(){
      if(is.character(self$number)){
        self$number
      } else {
        sprintf('%s.h5', self$number)
      }
    },

    #' @field fst_fname 'FST' file name
    fst_fname = function(){
      if(is.character(self$number)){
        self$number
      } else {
        sprintf('%s.fst', self$number)
      }
    },

    #' @field reference_equals_cached whether referenced data has been cached
    #' in 'RAVE' folder
    reference_equals_cached = function(){
      if(self$is_reference){
        return(TRUE)
      }

      ref <- self$reference$number
      if(is.numeric(ref)){
        ref <- sprintf('ref_%.0f', ref)
      }
      has_cached <- any(sprintf('%s%s', self$cached_reference, c('', '.h5')) %in% ref)
      has_cached
    },

    #' @field valid whether current electrode is valid: subject exists and
    #' contains current electrode or reference; subject electrode type matches
    #' with current electrode type
    valid = function(){
      if(!self$exists) {return(FALSE)}
      if(self$is_reference) {return(TRUE)}
      elec <- self$subject$electrodes
      if(!self$number %in% elec){ return(FALSE) }
      # type matches with subject
      if(!isTRUE(self$subject$electrode_types[elec %in% self$number] == self$type)){
        return(FALSE)
      }
      return(TRUE)
    },

    #' @field raw_sample_rate voltage sample rate
    raw_sample_rate = function(){
      elec <- self$subject$electrodes
      srate <- self$subject$raw_sample_rates[elec %in% self$number]
      if(!length(srate)){
        srate <- NA
      }
      srate
    },

    #' @field power_sample_rate power/phase sample rate
    power_sample_rate = function(){
      elec <- self$subject$electrodes
      srate <- self$subject$power_sample_rate[elec %in% self$number]
      if(!length(srate)){
        srate <- NA
      }
      srate
    },

    #' @field preprocess_info preprocess information
    preprocess_info = function(){
      self$subject$preprocess_settings$electrode_info(electrode = self$number)
    }

  )
)


#' Definition for electrodes that collects 'LFP' signals
#' @examples
#' \dontrun{
#'
#' # Download demo subject DemoSubject ~700MB data
#'
#' # Electrode 14
#' e <- LFP_electrode$new(subject = 'demo/DemoSubject',
#'                        number = 14, is_reference = FALSE)
#'
#' # Reference "ref_13-16,24"
#' ref <- LFP_electrode$new(subject = 'demo/DemoSubject',
#'                          number = "ref_13-16,24", is_reference = TRUE)
#'
#' # ------ Reference ------
#' # By default there is no reference
#' e$reference_name     # "noref
#'
#' # set reference
#' e$set_reference(reference = ref)
#' e$reference_name     # "ref_13-16,24"
#'
#' # Set epoch
#' e$set_epoch(epoch = 'auditory_onset')
#'
#' # Now epoch power
#' power <- e$epoch_power(before_onset = 1, after_onset = 2)
#'
#' # Trial x Frequency x Time x Electrodes
#' power
#'
#' # Subset power
#' subset(power, Time ~ Time < 0, Electrode ~ Electrode == 14)
#'
#' # clear memory in RAM and cache on hard disk
#' e$clear_cache()
#' e$clear_memory()
#'
#'
#' }
#' @export
LFP_electrode <- R6::R6Class(
  classname = 'LFP_electrode',
  inherit = Continuous_Electrode,
  cloneable = FALSE,
  lock_class = TRUE,
  public = list(
    #' @field type type of signals collected by the electrode
    type = 'LFP'
  )
)
