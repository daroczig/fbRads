library(testthat)

# 1 Remove one argument at the time and see if the function still works. The only compulsory argument is adset_id ----
adset_id = "23845914241040648"
ad_id = "23845914241050648"

expected_output = "copied_ad_id"

# 1.1 All arguments
expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_prefix = "All arguments"),
              
              expected_output)

# 1.2 No adset_id
expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_prefix = "No adset_id"),
              
              expected_output)

# 1.3 no status option
expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           rename_strategy = "DEEP_RENAME",
                           rename_prefix = "no status option"),
              
              expected_output)

# 1.4 no remame strategy
expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id),
              
              expected_output)


# 2 Test wrong argument types and see what errors are thrown ----
# 2.1 Wrong ad set id 
message = "This ad set id does not exists."

expect_error(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = "432432432",
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_prefix = "Morning-2"),
              
               message)

# 2.2 Wrong ad id 
message = "This ad id does not exists."

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = "321321321",
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME",
                          rename_prefix = "Morning-2"),
             
             message)

# 2.3 Mispelled status option 
message = "Some critical FB query error here."

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVEE",
                          rename_strategy = "DEEP_RENAME",
                          rename_prefix = "Morning-2"),
             
             message)

# 2.4 Mispelled rename strategy 
message = "Some critical FB query error here."

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVEE",
                          rename_strategy = "DEEP_RENAMEE",
                          rename_prefix = "Morning-2"),
             
             message)

# 3 Missing ad id -----
message = "Argument missing. An ad id is required."

expect_error(fbad_copy_ad(fbacc, 
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME",
                          rename_prefix = "Morning-2"),
             
             message)

# 4 --------
# 4.1 Rename strategy tests - DEEP_RENAME rename strategy without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME"),
             
             message)

# 4.1 Rename strategy tests - no rename strategy with prefix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_prefix = "Prefix"),
             
             message)

# 4.1.1 Rename strategy tests - no rename strategy with suffix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_suffix = "Suffix"),
             
             message)

# 4.1.2 Rename strategy tests - no rename strategy with prefix or suffix
message = "You have not selected a rename_strategy, therefore"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_prefix = "Prefix",
                          rename_suffix = "Suffix"),
             
             message)

# 4.2 Rename strategy tests - rename strategy == NO_RENAME with prefix
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "NO_RENAME",
                          rename_prefix = "Test"),
             
             message)

# 4.3 Rename strategy tests - rename strategy == NO_RENAME with suffix
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "NO_RENAME",
                          rename_suffix = "Suffix"),
             
             message)

# 4.3.1 Rename strategy tests - rename strategy == NO_RENAME with suffix AND PREFIX
message = "Your rename_stratey is 'NO_RENAME', therefore"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "NO_RENAME",
                          rename_suffix = "Suffix",
                          rename_prefix = "Prefix"),
             
             message)

# 4.4 Rename strategy tests - rename strategy == DEEP_RENAME without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME"),
             
             message)

# 4.5 Rename strategy tests - rename strategy == DEEP_RENAME without prefix or suffix
message = "You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy"

expect_error(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "ONLY_TOP_LEVEL_RENAME"),
             
             message)

# 4.5.1 Rename strategy tests - leaving arguments out all together
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE"),
             
             expected_output)

# 4.5.2 Rename strategy tests - DEEP_RENAME with Suffix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_suffix = "DEEP_RENAME with Suffix"),
              
              expected_output)

# 4.5.3 Rename strategy tests - DEEP_RENAME with Prefix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_prefix = "DEEP_RENAME with Prefix"),
              
              expected_output)

# 4.5.4 Rename strategy tests - DEEP_RENAME with Prefix and Suffix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "DEEP_RENAME",
                           rename_suffix = "Suffix",
                           rename_prefix = "Prefix"),
              
              expected_output)

# 4.5.5 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Suffix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                           rename_suffix = "ONLY_TOP_LEVEL_RENAME with Suffix"),
              
              expected_output)

# 4.5.6 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Prefix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                           rename_prefix = "ONLY_TOP_LEVEL_RENAME with Prefix"),
              
              expected_output)

# 4.5.7 Rename strategy tests - ONLY_TOP_LEVEL_RENAME with Prefix and Suffix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "ONLY_TOP_LEVEL_RENAME",
                           rename_suffix = "Suffix",
                           rename_prefix = "Prefix"),
              
              expected_output)

# 4.5.8 Rename strategy tests - NO_RENAME without Prefix and Suffix
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "ACTIVE",
                           rename_strategy = "NO_RENAME"),
              
              expected_output)

# 4.6 status options == "ACTIVE"
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                          ad_id = ad_id,
                          adset_id = adset_id,
                          status_option = "ACTIVE",
                          rename_strategy = "DEEP_RENAME",
                          rename_suffix = "Status==Active"),
             
              expected_output)

# 4.6.1 tatus options == "PAUSED"
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "PAUSED",
                           rename_strategy = "DEEP_RENAME",
                           rename_suffix = "Status==Paused"),
              
              expected_output)

# 4.6.2 tatus options == "INHERITED_FROM_SOURCE"
expected_output = "copied_ad_id"

expect_output(fbad_copy_ad(fbacc, 
                           ad_id = ad_id,
                           adset_id = adset_id,
                           status_option = "INHERITED_FROM_SOURCE",
                           rename_strategy = "DEEP_RENAME",
                           rename_suffix = "Status==INHERITED_FROM_SOURCE"),
              
              expected_output)


