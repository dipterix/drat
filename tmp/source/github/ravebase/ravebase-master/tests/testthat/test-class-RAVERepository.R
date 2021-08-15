library(testthat)
library(ravebase)
# devtools::load_all()
test_that("Test Repository", {
  skip_on_cran()

  # skip if demo subject is missing
  root <- ravebase:::rave_options('data_dir')
  skip_if_not(dir.exists(file.path(root, 'demo' ,'DemoSubject')), message = 'Demo subject not found, skipping')

  raveio::raveio_setopt('verbose_level', 'WARNING', .save = FALSE)

  before_onset <- 1
  after_onset <- 2
  epoch_name <- 'auditory_onset'
  reference_name <- 'default'

  rave_attach('demo/DemoSubject',
              reference = reference_name,
              epoch = epoch_name, before_onset = before_onset,
              after_onset = after_onset, electrodes = 1:20)

  expect_true(rave_attached$any_attached())

  repo <- rave_attached$get_repository()
  expect_is(repo, 'RAVERepository')

  expect_true(epoch_name %in% rave_attached$epoch_names())
  expect_true(reference_name %in% rave_attached$reference_names())

  expect_is(rave_attached$get_meta('electrodes'), 'data.frame')
  expect_is(rave_attached$get_meta('time_points'), 'data.frame')
  expect_is(rave_attached$get_meta('frequencies'), 'data.frame')
  expect_is(rave_attached$get_meta('trials'), 'data.frame')

  e <- repo$subject$electrodes
  expect_setequal(rave_attached$get_valid_electrodes(), e[!e %in% repo$ignored_electrodes])

  expect_setequal(rave_attached$get_loaded_electrodes(), repo$preload_electrodes)

  repo$clear_cache()

  for(dtype in c('power', 'phase', 'voltage')){
    rave_attached[[paste0('get_', dtype)]]()
    for(e in rave_attached$get_loaded_electrodes()){
      expect_true(
        repo$electrodes[[e]]$is_cached(
          before_onset = before_onset,
          after_onset = after_onset,
          dtype = dtype
        )
      )
    }
  }

  expect_equal(rave_attached$get_sample_rate(time_freq = TRUE), 100)

  expect_equal(rave_attached$get_sample_rate(time_freq = FALSE, type = 'ECoG'), 2000)
  expect_equal(rave_attached$get_sample_rate(time_freq = FALSE, type = 'LFP'), 2000)
  expect_identical(rave_attached$get_sample_rate(time_freq = FALSE, type = 'EEG'), NA)
  expect_identical(rave_attached$get_sample_rate(time_freq = FALSE, type = 'Spike'), NA)

  re <- rave_attached$get_subject('demo/DemoSubject')
  expect_true(re$identical)
  expect_true(re$attached)

  re <- rave_attached$get_preload_info()
  expect_equal(re$reference_name, reference_name)
  expect_equal(re$epoch_name, epoch_name)
  expect_length(re$time_points_power, (after_onset + before_onset) * 100 + 1)
  expect_length(re$condition, 17)
  expect_setequal(re$electrodes, repo$get_valid_electrodes(1:20))

})
