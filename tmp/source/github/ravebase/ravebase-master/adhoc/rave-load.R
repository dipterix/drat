
rave_attach('demo/YAB',
          reference = 'default',
          epoch = 'YABaOutlier', before_onset = 1,
          after_onset = 2, electrodes = 13:14)
repo <- rave_attached$get_repository()
repo$clear_cache()
rave_attached$get_power()



phase <- repo$epoch_continuous_signals(electrodes = 13:20, dtype = 'phase')
voltage <- repo$epoch_continuous_signals(electrodes = 13:20, dtype = 'voltage')
self = repo$electrodes[[13]]

phase = self$epoch_phase(1,2)

image(power[,,1,13])

# get repository in other place instead of creating a new one
repo2 <-
  loaded_rave_repository(
    subject = 'demo/DemoSubject',
    reference = 'default',
    epoch = 'auditory_onset',
    before_onset = 1,
    after_onset = 2
  )

identical(repo2, repo)

# remove disk data
repo$clear_cache()

# release RAM
repo$clear_memory()
pryr::object_size(repo)
