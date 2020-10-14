source("Clients/Data Science/fbRads-Arben/R/fb_copy_adset.R")

# 1 Remove one argument at the time and see if the function still works. The only compulsory argument is adset_id ----
expected = "{\"copied_adset_id\""

# 1.1
expect_equal(fbad_copy_adset(fbacc, 
                              adset_id = "23845893193900648",
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Morning-2",
                              rename_suffix = "Hello"),
             
             expected)

# 1.2
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648",
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2"),
             
             expected)

# 1.3
error = "You have selected 'DEEP_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both"

expect_error(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648",
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME"),
             
                   error)

# 1.4
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648",
                             deep_copy = TRUE,
                             start_time = "start_time",
                             end_time = end_time,
                             status_option = "ACTIVE"),
             
             expected)

# 1.5
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648",
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time),
             
             expected)

# 1.6
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648",
                             deep_copy = TRUE,
                             start_time = start_time),
             
             expected)

# 1.7
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648",
                             deep_copy = TRUE),
             
             expected)

# 1.8
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = "23845893193900648"),
             
             expected)

# 1.9 campaign id
expect_equal(fbad_copy_adset(fbacc, 
                adset_id = "23845893193900648",
                deep_copy = TRUE,
                status_option = "ACTIVE",
                rename_strategy = "DEEP_RENAME",
                rename_prefix = "Morning-2"),
                expected)

# 2 Test wrong argument types and see what errors are thrown ----

# 2.1 deep copy
error = "Some critical FB query error here."

expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = "TRUEE",
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.2 start_time
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = "start_time",
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.3 start_time
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = "end_time",
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.4 status_option
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "TRUE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)


# 2.4 rename_strategy
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "TRUE",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.4 rename_strategy
expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "TRUE",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "NO_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "ONLY_TOP_LEVEL_RENAME"),
             
             error)

expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME"),
             
             error)

expect_equal(fbad_copy_adset(fbacc, 
                             adset_id = 23845893193900648,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "testthat"),
             
             error)

# 3 Test if adset_id is missing ----
error = "Argument missing. An adset id is required."

# 3.1 missing ad set
error = "Argument missing. An adset id is required."

expect_error(fbad_copy_adset(fbacc, 
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "testthat"),
             
             error)

# 3.2 not an ad set id
error = "This adset id does not exists. Please provide a valid adset id."
expect_error(fbad_copy_adset(fbacc,
                             adset_id = 34234234,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "testthat"),
             
             error)

# 3.3 not a campaign id
error = "This campaign id does not exists. Please provide a valid campaign id."
expect_error(fbad_copy_adset(fbacc,
                             adset_id = 23845893193900648,
                             campaign_id = 342423423423,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "testthat"),
             
             error)
             








