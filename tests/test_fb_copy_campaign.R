library(testthat)

campaign_id = "23845893051630648"

# 1 remove one argument at the time and see if it still works
# 1.1.1 remove rename_suffix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                campaign_id = campaign_id,
                                deep_copy = TRUE,
                                start_time = start_time,
                                end_time = end_time,
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Morning-2"),
             
             expected_output)

# 1.1.2 remove rename_prefix
expect_output(fbad_copy_campaign(fbacc, 
                                 campaign_id = campaign_id,
                                 deep_copy = TRUE,
                                 start_time = start_time,
                                 end_time = end_time,
                                 status_option = "ACTIVE",
                                 rename_strategy = "DEEP_RENAME",
                                 rename_suffix = "Late"),
              
              expected_output)

# 1.1.3 remove rename_strategy
expect_output(fbad_copy_campaign(fbacc, 
                                 campaign_id = campaign_id,
                                 deep_copy = TRUE,
                                 start_time = start_time,
                                 end_time = end_time,
                                 status_option = "ACTIVE"),
              
              expected_output)

# 1.1.4 remove status option
expect_output(fbad_copy_campaign(fbacc, 
                                 campaign_id = campaign_id,
                                 deep_copy = TRUE,
                                 start_time = start_time,
                                 end_time = end_time),
              
              expected_output)

# 1.1.5 remove end time
expect_output(fbad_copy_campaign(fbacc, 
                                 campaign_id = campaign_id,
                                 deep_copy = TRUE,
                                 start_time = start_time),
              
              expected_output)

# 1.1.6 remove start time
expect_output(fbad_copy_campaign(fbacc, 
                                 campaign_id = campaign_id,
                                 deep_copy = TRUE),
              
              expected_output)

# 1.1.6 remove deep_copy
expect_output(fbad_copy_campaign(fbacc, 
                                 campaign_id = campaign_id),
              
              expected_output)

# 2 Create tests that should theoretically generate an error ----

# 2.1.1 wrong campaign id
expected_error = "This campaign id does not exists"

expect_error(fbad_copy_campaign(fbacc, 
                                 campaign_id = 4324324342432,
                                 deep_copy = TRUE,
                                 start_time = start_time,
                                 end_time = end_time,
                                 status_option = "ACTIVE",
                                 rename_strategy = "DEEP_RENAME",
                                 rename_prefix = "Morning-2"),
              
             expected_error)

# 2.1.2 wrong deep copy argument
expected_error = "Some critical FB query error here."

expect_error(fbad_copy_campaign(fbacc, 
                                campaign_id = campaign_id,
                                deep_copy = "TRUEE",
                                start_time = start_time,
                                end_time = end_time,
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Morning-2"),
             
             expected_error)

# 2.1.3 wrong start_time argument
expected_error = "Some critical FB query error here."

expect_error(fbad_copy_campaign(fbacc, 
                                campaign_id = campaign_id,
                                deep_copy = TRUE,
                                start_time = "start_time",
                                end_time = end_time,
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Morning-2"),
             
             expected_error)

# 2.1.4 wrong end_time argument
expected_error = "Some critical FB query error here."

expect_error(fbad_copy_campaign(fbacc, 
                                campaign_id = campaign_id,
                                deep_copy = TRUE,
                                start_time = start_time,
                                end_time = "end_time",
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Morning-2"),
             
             expected_error)

# 2.1.5 wrong status_options argument
expected_error = "Some critical FB query error here."

expect_error(fbad_copy_campaign(fbacc, 
                                campaign_id = campaign_id,
                                deep_copy = TRUE,
                                start_time = start_time,
                                end_time = end_time,
                                status_option = "ACTIVEE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Morning-2"),
             
             expected_error)

# 2.1.6 wrong rename_strategy argument
expected_error = "Some critical FB query error here."

expect_error(fbad_copy_campaign(fbacc, 
                                campaign_id = campaign_id,
                                deep_copy = TRUE,
                                start_time = start_time,
                                end_time = end_time,
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAMEE",
                                rename_prefix = "Morning-2"),
             
             expected_error)

# 3 Test missing campaign id argument ------
expected_error = "Argument missing. A campaign id is required."

expect_error(fbad_copy_campaign(fbacc, 
                                deep_copy = TRUE,
                                start_time = start_time,
                                end_time = end_time,
                                status_option = "ACTIVE",
                                rename_strategy = "DEEP_RENAME",
                                rename_prefix = "Morning-2"),
             
             expected_error)

# 4 Test various different rename_strategy combinations ----
# 4.1 Rename strategy tests - DEEP_RENAME rename strategy without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME"),
             
             message)

# 4.1 Rename strategy tests - no rename strategy with prefix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_prefix = "Prefix"),
             
             message)

# 4.1.1 Rename strategy tests - no rename strategy with suffix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_suffix = "Suffix"),
             
             message)

# 4.1.2 Rename strategy tests - no rename strategy with prefix or suffix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_prefix = "Prefix",
                             rename_suffix = "Suffix"),
             
             message)

# 4.2 Rename strategy tests - rename strategy == NO_RENAME with prefix
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_strategy = "NO_RENAME",
                             rename_prefix = "Test"),
             
             message)

# 4.3 Rename strategy tests - rename strategy == NO_RENAME with suffix
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_strategy = "NO_RENAME",
                             rename_suffix = "Suffix"),
             
             message)

# 4.3.1 Rename strategy tests - rename strategy == NO_RENAME with suffix AND PREFIX
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_strategy = "NO_RENAME",
                             rename_suffix = "Suffix",
                             rename_prefix = "Prefix"),
             
             message)

# 4.4 Rename strategy tests - rename strategy == DEEP_RENAME without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_strategy = "DEEP_RENAME"),
             
             message)

# 4.5 Rename strategy tests - rename strategy == DEEP_RENAME without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy"

expect_error(fbad_copy_campaign(fbacc, 
                             campaign_id = campaign_id,
                             status_option = "ACTIVE",
                             rename_strategy = "ONLY_TOP_LEVEL_RENAME"),
             
             message)

# 4.5.1 Rename strategy tests - leaving arguments out all together
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              status_option = "ACTIVE"),
              
              expected_output)

# 4.5.2 Rename strategy tests - DEEP_RENAME with Suffix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                 deep_copy = TRUE,
                              campaign_id = campaign_id,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_suffix = "DEEP_RENAME with Suffix"),
              
              expected_output)

# 4.5.3 Rename strategy tests - DEEP_RENAME with Prefix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                 deep_copy = TRUE,
                              campaign_id = campaign_id,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "DEEP_RENAME with Prefix"),
              
              expected_output)

# 4.5.4 Rename strategy tests - DEEP_RENAME with Prefix and Suffix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                 deep_copy = TRUE,
                                 campaign_id = campaign_id,
                                 status_option = "ACTIVE",
                                 rename_strategy = "DEEP_RENAME",
                                 rename_suffix = "Suffix",
                                 rename_prefix = "Prefix"),
              
              expected_output)

# 4.5.5 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Suffix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                 deep_copy = TRUE,
                              campaign_id = campaign_id,
                              status_option = "ACTIVE",
                              rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                              rename_suffix = "ONLY_TOP_LEVEL_RENAME with Suffix"),
              
              expected_output)

# 4.5.6 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Prefix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                 deep_copy = TRUE,
                              campaign_id = campaign_id,
                              status_option = "ACTIVE",
                              rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                              rename_prefix = "ONLY_TOP_LEVEL_RENAME with Prefix"),
              
              expected_output)

# 4.5.7 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Prefix and Suffix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                                 deep_copy = TRUE,
                              campaign_id = campaign_id,
                              status_option = "ACTIVE",
                              rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                              rename_suffix = "Suffix",
                              rename_prefix = "Prefix"),
              
              expected_output)

# 4.5.8 Rename strategy tests - NO_RENAME without Prefix and Suffix
expected_output = "copied_campaign_id"

expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              status_option = "ACTIVE",
                              rename_strategy = "NO_RENAME"),
              
              expected_output)

# 5 test the different arguments -----
# 5.1
expected_output = "copied_campaign_id"
# 5.3.1 deep copy == TRUE
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "deep copy true"),
              
              expected_output)

# 5.3.2 deep copy == FALSE
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = FALSE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "deep copy false"),
              
              expected_output)

# 5.4.1 only start time
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Only start"),
              
              expected_output)

# 5.4.2 only end time
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Only end"),
              
              expected_output)

# 5.4.3 both start time and end time
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Both start end"),
              
              expected_output)

# 5.5.1 status == active
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "ACTIVE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "Active"),
              
              expected_output)

# 5.5.2 status == paused
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "PAUSED",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "PAUSED"),
              
              expected_output)

# 5.5.3 status == INHERITED_FROM_SOURCE
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "INHERITED_FROM_SOURCE"),
              
              expected_output)

# 5.6.1 rename strategy == "DEEP_RENAME"
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "DEEP_RENAME",
                              rename_prefix = "DEEP_RENAME"),
              
              expected_output)

# 5.6.2 rename strategy == "ONLY_TOP_LEVEL_RENAME"
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                              rename_prefix = "ONLY_TOP_LEVEL_RENAME"),
              
              expected_output)

# 5.6.3 rename strategy == "NO_RENAME"
expect_output(fbad_copy_campaign(fbacc, 
                              campaign_id = campaign_id,
                              deep_copy = TRUE,
                              start_time = start_time,
                              end_time = end_time,
                              status_option = "INHERITED_FROM_SOURCE",
                              rename_strategy = "NO_RENAME"),
              
              expected_output)

