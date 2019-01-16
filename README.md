# fbRads

This R package includes wrapper functions around the [Facebook Marketing API](https://developers.facebook.com/docs/marketing-apis) to create, read, update and delete custom audiences, images, campaigns, adsets, ads and related content.

For more details, see the [slides](http://bit.ly/domino-webinar-fbRads) presented at [useR! 2015](http://user2015.math.aau.dk/contributed_talks#210), [EARL 2015](https://earlconf.com/2015/boston/speakers/speaker.php?s=gergely_daroczi), the [Los Angeles R Users Group](http://www.meetup.com/Los-Angeles-R-Users-Group-Data-Science/events/226717454/) and at a [Domino Webinar](https://www.youtube.com/watch?v=RY9pfnBRi-Q).

## Creating a Facebook App & connect with OAuth token

To be able to use this package, you will have to create a Facebook App and authorize it to manage your Facebook ads. Basic steps to create an app with Development access level letting you manage up to 5 Facebook ad accounts:

1. Click on "Add a New App" at https://developers.facebook.com/apps
2. Fill in a unique `Display Name` (eg "app_testing_foobar_42"), and provide your e-mail address. Click on "Create App ID" & pass the captcha test.
3. Select the "Implement Marketing API" scenario that will automatically add the "Marketing API" to the "Products" section of the sidebar, or click on the "+" butting in the "Products" section of the sidebar
4. Optionally associate your app with a Business Manager in the "Settings" screen of the "Marketing API"
5. In the sidebar, select "Tools" from the "Marketing API" and generate a token
6. Store your token in a secure place, and you are all set to start using `fbRads`, eg list all the Ad Account ids you can access:

    ```r
    accounts <- fbad_get_my_ad_accounts(token, version = '3.1')
    ```

7. Pick an Ad Account id from the returned list and initialize `fbRads` to use that Ad Account by default:

    ```r
    account <- sample(accounts$account_id, 1)
    fbad_init(accountid = account, token = token, version = '3.1')
    ```

8. Then list all your ads along with the ad name and status on that Ad Account:

    ```r
    fbad_list_ad(fields = c('name', 'effective_status'))
    ```

9. Or eg filter for the active ads:

   ```r
   fbad_list_ad(statuses = 'ACTIVE', fields = 'name')
   ```

## Development version

The package is actively maintained, but not frequently pushed to CRAN, so to use the most recent version, install from GitHub. The `master` branch is supposed to include a relatively stable version at all the time:

```r
devtools::install_github('daroczig/fbRads')
```

## Using the package

This package makes your life more convenient when it comes to interacting with the Facebook Marketing API, but unfortunately, it cannot save you the time to get familiar with the actual API endpoints. To be able to interact with Facebook, you have to learn about how the API works etc -- see the documentation at <https://developers.facebook.com/docs/marketing-apis>

---

This package was originally developed at [CARD.com](http://card.com), currently maintained at [System1](https://system1.com) -- licensed under [AGPL-3](LICENSE).
