context("Themes")

# ###################################################################################
# VALID ggplot2 themes
test_that("get_theme of ggplot2 themes", {
    
    # number of expected returned list items (corresponding to valid_themes above)
    valid_themes_lengths <- c(7,7,9,9,7,7,7,9,7)
    names(valid_themes_lengths) <- c("gray", "grey", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")
    
    # make sure get_theme() returns a list object
    # SCENARIO: Valid ggplot2 themes
    # EXPECT: Successfully return list object
    expect_is(get_theme("gray"), "list")
    expect_equal(length(get_theme("gray")), valid_themes_lengths[["gray"]])
    expect_is(get_theme("grey"), "list")
    expect_equal(length(get_theme("grey")), valid_themes_lengths[["grey"]])
    expect_is(get_theme("bw"), "list")
    expect_equal(length(get_theme("bw")), valid_themes_lengths[["bw"]])
    expect_is(get_theme("linedraw"), "list")
    expect_equal(length(get_theme("linedraw")), valid_themes_lengths[["linedraw"]])
    expect_is(get_theme("light"), "list")
    expect_equal(length(get_theme("light")), valid_themes_lengths[["light"]])
    expect_is(get_theme("dark"), "list")
    expect_equal(length(get_theme("dark")), valid_themes_lengths[["dark"]])
    expect_is(get_theme("minimal"), "list")
    expect_equal(length(get_theme("minimal")), valid_themes_lengths[["minimal"]])
    expect_is(get_theme("classic"), "list")
    expect_equal(length(get_theme("classic")), valid_themes_lengths[["classic"]])
    expect_is(get_theme("void"), "list")
    expect_equal(length(get_theme("void")), valid_themes_lengths[["void"]])
    
})

# ###################################################################################
# VALID ggthemes themes
test_that("get_theme of ggthemes themes", {
    
    if ("package:ggthemes" %in% search()) {
        detach("package:ggthemes", unload=TRUE)    
    }
 
    # number of expected returned list items (corresponding to valid_ggthemes above)
    valid_ggthemes_lengths <- c(9,9,7,7,9,9,7,9,7,7,9,7,7,7,9)
    names(valid_ggthemes_lengths) <- c("base", "calc", "economist", "economist_white", "excel", "few", "fivethirtyeight", 
                                       "gdocs", "hc", "pander", "solarized", "solarized2", "stata", "tufte", "wsj")
    
    # make sure get_theme() throws error
    # SCENARIO: Valid ggthemes themes without loading ggthemes package
    # EXPECT: Error is thrown
    expect_error(get_theme("base"))
    expect_error(get_theme("calc"))
    expect_error(get_theme("economist"))
    expect_error(get_theme("economist_white"))
    expect_error(get_theme("excel"))
    expect_error(get_theme("few"))
    expect_error(get_theme("fivethirtyeight"))
    expect_error(get_theme("gdocs"))
    expect_error(get_theme("hc"))
    expect_error(get_theme("pander"))
    expect_error(get_theme("solarized"))
    expect_error(get_theme("solarized2"))
    expect_error(get_theme("stata"))
    expect_error(get_theme("tufte"))
    expect_error(get_theme("wsj"))
    
    # make sure get_theme() returns a list object
    # SCENARIO: Valid ggthemes themes with ggthemes package loaded
    # EXPECT: Successfully return list object
    library(ggthemes)
    expect_is(get_theme("base"), "list")
    expect_equal(length(get_theme("base")), valid_ggthemes_lengths[["base"]])
    expect_is(get_theme("calc"), "list")
    expect_equal(length(get_theme("calc")), valid_ggthemes_lengths[["calc"]])
    expect_is(get_theme("economist"), "list")
    expect_equal(length(get_theme("economist")), valid_ggthemes_lengths[["economist"]])
    expect_is(get_theme("economist_white"), "list")
    expect_equal(length(get_theme("economist_white")), valid_ggthemes_lengths[["economist_white"]])
    expect_is(get_theme("excel"), "list")
    expect_equal(length(get_theme("excel")), valid_ggthemes_lengths[["excel"]])
    expect_is(get_theme("few"), "list")
    expect_equal(length(get_theme("few")), valid_ggthemes_lengths[["few"]])
    expect_is(get_theme("fivethirtyeight"), "list")
    expect_equal(length(get_theme("fivethirtyeight")), valid_ggthemes_lengths[["fivethirtyeight"]])
    expect_is(get_theme("gdocs"), "list")
    expect_equal(length(get_theme("gdocs")), valid_ggthemes_lengths[["gdocs"]])
    expect_is(get_theme("hc"), "list")
    expect_equal(length(get_theme("hc")), valid_ggthemes_lengths[["hc"]])
    expect_is(get_theme("pander"), "list")
    expect_equal(length(get_theme("pander")), valid_ggthemes_lengths[["pander"]])
    expect_is(get_theme("solarized"), "list")
    expect_equal(length(get_theme("solarized")), valid_ggthemes_lengths[["solarized"]])
    expect_is(get_theme("solarized2"), "list")
    expect_equal(length(get_theme("solarized2")), valid_ggthemes_lengths[["solarized2"]])
    expect_is(get_theme("stata"), "list")
    expect_equal(length(get_theme("stata")), valid_ggthemes_lengths[["stata"]])
    expect_is(get_theme("tufte"), "list")
    expect_equal(length(get_theme("tufte")), valid_ggthemes_lengths[["tufte"]])
    expect_is(get_theme("wsj"), "list")
    expect_equal(length(get_theme("wsj")), valid_ggthemes_lengths[["wsj"]])
   
})

# ###################################################################################
# CUSTOM themes
test_that("get_theme of custom theme(s)", {
    
    # make sure get_theme() returns a list object
    expect_is(get_theme("custom"), "list")
    
    # make sure get_theme() issues warning when given invalid values
    expect_warning(get_theme("custom", bg="notacolor"))
    expect_warning(get_theme("custom", bg="#AGEB"))
    expect_warning(get_theme("custom", fg="somebadcolor"))
    expect_warning(get_theme("custom", fg="#AA00"))
    expect_warning(get_theme("custom", base_size=-5))
    expect_warning(get_theme("custom", base_size="notanumber"))
    expect_warning(get_theme("custom", base_family=5))
    expect_warning(get_theme("custom", base_family="Times New Roman"))
    expect_warning(get_theme("custom", cex=-3))
    expect_warning(get_theme("custom", cex="notanumberbutastring!"))
    expect_warning(get_theme("custom", fill="anotherbadone"))
    expect_warning(get_theme("custom", col="somebadcolor"))
    expect_warning(get_theme("custom", col="#FF"))
    expect_warning(get_theme("custom", lwd="five"))
    expect_warning(get_theme("custom", lwd=-6))
    expect_warning(get_theme("custom", lty="notalinetype"))
    expect_warning(get_theme("custom", lty=10))
    expect_warning(get_theme("custom", lty=-3))
    
    # make sure get_theme() returns a list object when given valid values
    expect_is(get_theme("custom", bg="black"), "list")
    expect_is(get_theme("custom", bg="#ABABAB"), "list")
    expect_is(get_theme("custom", fg="red"), "list")
    expect_is(get_theme("custom", fg="#00AA55"), "list")
    expect_is(get_theme("custom", base_size=5), "list")
    expect_is(get_theme("custom", base_family="mono"), "list")
    expect_is(get_theme("custom", cex=3), "list")
    expect_is(get_theme("custom", fill="blue"), "list")
    expect_is(get_theme("custom", fill="#ACCA44"), "list")
    expect_is(get_theme("custom", col="blue"), "list")
    expect_is(get_theme("custom", col="#ACCA44"), "list")
    expect_is(get_theme("custom", lty="dotted"), "list")
    expect_is(get_theme("custom", lty=4), "list")
})

# ###################################################################################
# INVALID themes
test_that("get_theme of invalid theme(s)", {
    
    expect_error(get_theme("nyt"))
    expect_error(get_theme("newyorker"))
    expect_error(get_theme("testing"))
    expect_error(get_theme("abcd"))

})