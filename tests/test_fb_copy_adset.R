library(testthat)

campaign_id = "23845914189980648"
adset_id = "23845914241040648"

# 1 Remove one argument at the time and see if the function still works. The only compulsory argument is adset_id ----
expected = "{\"copied_adset_id\""

# 1.1
expect_equal(fbad_copy_adsetset(fbacc, 
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Morning-2",
                              rename_suffix = "Hello"),
             
             expected)

# 1.2
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2"),
             
             expected)

# 1.4
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = "start_time",
                             end_time = end_time,
                             status_option = "ACTIVE"),
             
             expected)

# 1.5
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time),
             
             expected)

# 1.6
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = start_time),
             
             expected)

# 1.7
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE),
             
             expected)

# 1.8
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id),
             
             expected)

# 1.9 campaign id
expect_equal(fbad_copy_adsetset(fbacc, 
                adset_id = adset_id,
                deep_copy = TRUE,
                status_option = "ACTIVE",
                rename_strategy = "DEEP_RENAME",
                rename_prefix = "Morning-2"),
                expected)

# 2 Test wrong argument types and see what errors are thrown ----

# 2.1 deep copy
error = "Some critical FB query error here."

expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = "TRUEE",
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.2 start_time
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = "start_time",
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.3 end_time
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = "end_time",
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 2.4 status_option
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "TRUE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)


# 2.4 rename_strategy
expect_equal(fbad_copy_adsetset(fbacc, 
                             adset_id = adset_id,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "TRUE",
                             rename_prefix = "Morning-2",
                             rename_suffix = "Hello"),
             
             error)

# 3 Test if adset_id is missing ----
# 3.1 missing ad set
error = "Argument missing. An adset id is required."

expect_error(fbad_copy_adsetset(fbacc, 
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "testthat"),
             
             error)

# 3.2 not an ad set id
error = "This adset id does not exists. Please provide a valid adset id."
expect_error(fbad_copy_adsetset(fbacc,
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
expect_error(fbad_copy_adsetset(fbacc,
                             adset_id = adset_id,
                             campaign_id = 342423423423,
                             deep_copy = TRUE,
                             start_time = start_time,
                             end_time = end_time,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME",
                             rename_prefix = "testthat"),
             
             error)
             
# 4 --------
# 4.1 Rename strategy tests - DEEP_RENAME rename strategy without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME"),
             
             message)

# 4.1 Rename strategy tests - no rename strategy with prefix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_prefix = "Prefix"),
             
             message)

# 4.1.1 Rename strategy tests - no rename strategy with suffix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_suffix = "Suffix"),
             
             message)

# 4.1.2 Rename strategy tests - no rename strategy with prefix or suffix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_prefix = "Prefix",
                          rename_suffix = "Suffix"),
             
             message)

# 4.2 Rename strategy tests - rename strategy == NO_RENAME with prefix
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "NO_RENAME",
                          rename_prefix = "Test"),
             
             message)

# 4.3 Rename strategy tests - rename strategy == NO_RENAME with suffix
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "NO_RENAME",
                          rename_suffix = "Suffix"),
             
             message)

# 4.3.1 Rename strategy tests - rename strategy == NO_RENAME with suffix AND PREFIX
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "NO_RENAME",
                          rename_suffix = "Suffix",
                          rename_prefix = "Prefix"),
             
             message)

# 4.4 Rename strategy tests - rename strategy == DEEP_RENAME without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME"),
             
             message)

# 4.5 Rename strategy tests - rename strategy == DEEP_RENAME without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy"

expect_error(fbad_copy_adset(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "ONLY_TOP_LEVEL_RENAME"),
             
             message)

# 4.5.1 Rename strategy tests - leaving arguments out all together
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE"),
              
              expected_output)

# 4.5.2 Rename strategy tests - DEEP_RENAME with Suffix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_suffix = "DEEP_RENAME with Suffix"),
              
              expected_output)

# 4.5.3 Rename strategy tests - DEEP_RENAME with Prefix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_prefix = "DEEP_RENAME with Prefix"),
              
              expected_output)

# 4.5.4 Rename strategy tests - DEEP_RENAME with Prefix and Suffix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_suffix = "Suffix",
                           rename_prefix = "Prefix"),
              
              expected_output)

# 4.5.5 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Suffix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                           rename_suffix = "ONLY_TOP_LEVEL_RENAME with Suffix"),
              
              expected_output)

# 4.5.6 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Prefix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                           rename_prefix = "ONLY_TOP_LEVEL_RENAME with Prefix"),
              
              expected_output)

# 4.5.7 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Prefix and Suffix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                           rename_suffix = "Suffix",
                           rename_prefix = "Prefix"),
              
              expected_output)

# 4.5.8 Rename strategy tests - NO_RENAME without Prefix and Suffix
expected_output = "copied_adset_id"

expect_output(fbad_copy_adset(fbacc, 
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "NO_RENAME"),
              
              expected_output)


# 5 Check if different arguments are properly working ----
expected_output = "copied_adset_id"
# 5.1 same campaign id in which the ad set is located
expect_output(fbad_copy_adset(fbacc, 
                                campaign_id = campaign_id,
                                adset_id = adset_id,
                                deep_copy = TRUE,
                                start_time = start_time,
                                end_time = end_time,
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Same campaign"),
             
             expected_output)

# 5.2 different campaign id in which the ad set is located
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = "23845893051630648",
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Different campaign"),
              
              expected_output)

# 5.3.1 deep copy == TRUE
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "deep copy true"),
              
              expected_output)

# 5.3.2 deep copy == FALSE
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = FALSE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "deep copy false"),
              
              expected_output)

# 5.4.1 only start time
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Only start"),
              
              expected_output)

# 5.4.2 only end time
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Only end"),
              
              expected_output)

# 5.4.3 both start time and end time
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Both start end"),
              
              expected_output)

# 5.5.1 status == active
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Active"),
              
              expected_output)

# 5.5.2 status == paused
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "PAUSED",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "PAUSED"),
              
              expected_output)

# 5.5.3 status == INHERITED_FROM_SOURCE
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "INHERITED_FROM_SOURCE"),
              
              expected_output)

# 5.6.1 rename strategy == "DEEP_RENAME"
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "DEEP_RENAME"),
              
              expected_output)

# 5.6.2 rename strategy == "ONLY_TOP_LEVEL_RENAME"
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                              rename_prefix = "ONLY_TOP_LEVEL_RENAME"),
              
              expected_output)

# 5.6.3 rename strategy == "NO_RENAME"
expect_output(fbad_copy_adset(fbacc, 
                              campaign_id = campaign_id,
                              adset_id = adset_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "NO_RENAME"),
              
              expected_output)




