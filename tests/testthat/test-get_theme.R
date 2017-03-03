context("Themes")

# ###################################################################################
# VALID ggplot2 themes
test_that("get_theme of ggplot2 themes", {
    
    # valid themes from ggplot2 package
    valid_themes <- c("gray", "grey", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")
    
    # number of expected returned list items (corresponding to valid_themes above)
    valid_themes_lengths <- c(7,7,9,9,7,7,7,9,7)
    names(valid_themes_lengths) <- valid_themes
    
    # make sure get_theme() returns a list object
    # SCENARIO: Valid ggplot2 themes
    # EXPECT: Successfully return list object
    for (t in valid_themes) {
        expect_is(get_theme(t), "list")
        expect_equal(length(get_theme(t)), valid_themes_lengths[[t]])
    }
})

# ###################################################################################
# VALID ggthemes themes
test_that("get_theme of ggthemes themes", {
    
    detach("package:ggthemes", unload=TRUE)
    
    # valid themes from ggthemes package
    valid_ggthemes <- c("base", "calc", "economist", "economist_white", "excel", "few", "fivethirtyeight", 
                        "gdocs", "hc", "pander", "solarized", "solarized2", "stata", "tufte", "wsj")
    
    # number of expected returned list items (corresponding to valid_ggthemes above)
    valid_ggthemes_lengths <- c(9,9,7,7,9,9,7,9,7,7,9,7,7,7,9)
    names(valid_ggthemes_lengths) <- valid_ggthemes
    
    # make sure get_theme() throws error
    # SCENARIO: Valid ggthemes themes without loading ggthemes package
    # EXPECT: Error is thrown
    for (t in valid_ggthemes) {
        expect_error(get_theme(t))
    }
    
    # make sure get_theme() returns a list object
    # SCENARIO: Valid ggthemes themes with ggthemes package loaded
    # EXPECT: Successfully return list object
    library(ggthemes)
    for (t in valid_ggthemes) {
        expect_is(get_theme(t), "list")
        expect_equal(length(get_theme(t)), valid_ggthemes_lengths[[t]])
    }   
})

# ###################################################################################
# CUSTOM themes
test_that("get_theme of custom theme(s)", {
    
    # valid custom theme
    valid_custom <- c("custom")
    
    # make sure get_theme() returns a list object
    for (t in valid_custom) {
        expect_is(get_theme(t), "list")
    }
    
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
    
    # invalid themes
    invalid <- c("nyt", "newyorker", "testing", "abcd")
    
    # make sure get_theme() throws error
    for (t in invalid) {
        expect_error(get_theme(t))
    }   
})