context("Plot documentation")

# ###################################################################################
# ensure error is thrown if ggplot object is not given for argument 'g'
test_that("error thrown if ggplot object is not given for argument 'g'", {

    # character string
    expect_error(doc_plot("not_a_ggplot_object"))
    
    # numeric
    expect_error(doc_plot(3))
    
    # true logical
    expect_error(doc_plot(TRUE))
    
    # false logical
    expect_error(doc_plot(FALSE))
    
    # base graphics plot
    expect_error(doc_plot(plot(diamonds$carat, diamonds$price)))
    
    # ggplot2 geom (not a complete ggplot object)
    expect_error(doc_plot(geom_point()))
    
})

# ###################################################################################
# ensure error is thrown if character is not given for argument 'author'
test_that("error thrown if character is not given for argument 'author'", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, author=3))
    
    # logical
    expect_error(doc_plot(g, author=TRUE))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, author=c("Joshua", "Poirier")))
    
    # data frame
    expect_error(doc_plot(g, author=data.frame(x=1, y=2)))
    
    # data frame with author as character in it
    expect_error(doc_plot(g, author=data.frame(author="Joshua Poirier")))
    
    # ggplot object
    expect_error(doc_plot(g, author=g))
    
    # ggplot2 geom object
    expect_error(doc_plot(g, author=geom_point()))

})

# ###################################################################################
# ensure error is thrown if character object is not given for argument 'author_title'
# BUT ONLY IF author argument is given
test_that("error thrown if character is not given for argument 'author_title'", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, author="Josh", author_title=3))
    
    # logical
    expect_error(doc_plot(g, author="Josh", author_title=TRUE))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, author="Josh", author_title=c("Scientist", "Programmer")))
    
    # data frame
    expect_error(doc_plot(g, author="Josh", author_title=data.frame(firstname="Josh", title="Programmer")))
    
    # ggplot object
    expect_error(doc_plot(g, author="Josh", author_title=g))
    
})

# ###################################################################################
# ensure error is thrown if character object is not given for argument 'data_source'
test_that("error thrown if character is not given for argument 'data_source'", {
 
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, data_source=3))
    
    # logical
    expect_error(doc_plot(g, data_source=TRUE))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, data_source=c("The scary internet!", "My local newspaper")))
    
    # data frame
    expect_error(doc_plot(g, data_source=data.frame(x=1, y=2)))
    
    # data frame with data source as character in it
    expect_error(doc_plot(g, data_source=data.frame(author="The scary internet!")))
    
    # ggplot object
    expect_error(doc_plot(g, data_source=g))
    
    # ggplot2 geom object
    expect_error(doc_plot(g, data_source=geom_point()))
       
})

# ###################################################################################
# ensure error is thrown if logical object is not given for argument 'date'
test_that("error thrown if logical is not given for argument 'date'", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, date=3))
    
    # logical
    expect_error(doc_plot(g, date="TRUE"))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, date=c("The scary internet!", "My local newspaper")))
    
    # data frame
    expect_error(doc_plot(g, date=data.frame(x=1, y=2)))
    
    # data frame with data source as character in it
    expect_error(doc_plot(g, date=data.frame(author="The scary internet!")))
    
    # ggplot object
    expect_error(doc_plot(g, date=g))
    
    # ggplot2 geom object
    expect_error(doc_plot(g, date=geom_point()))    
    
})

# ###################################################################################
# ensure error is thrown if character object is not given for argument 'img_sponsor'
test_that("error thrown if character is not given for argument 'img_sponsor'", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, img_sponsor=3))
    
    # logical
    expect_error(doc_plot(g, img_sponsor=TRUE))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, img_sponsor=c("My employer!", "My other employers!")))
    
    # data frame
    expect_error(doc_plot(g, img_sponsor=data.frame(x=1, y=2)))
    
    # data frame with data source as character in it
    expect_error(doc_plot(g, img_sponsor=data.frame(img_sponsor="My sponsor!")))
    
    # ggplot object
    expect_error(doc_plot(g, img_sponsor=g))
    
    # ggplot2 geom object
    expect_error(doc_plot(g, img_sponsor=geom_point()))
    
})

# ###################################################################################
# ensure error is thrown if character object is not given for argument 'sponsor'
test_that("error thrown if character is not given for argument 'sponsor'", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, sponsor=3))
    
    # logical
    expect_error(doc_plot(g, sponsor=TRUE))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, sponsor=c("My employer!", "My other employers!")))
    
    # data frame
    expect_error(doc_plot(g, sponsor=data.frame(x=1, y=2)))
    
    # data frame with data source as character in it
    expect_error(doc_plot(g, sponsor=data.frame(img_sponsor="My sponsor!")))
    
    # ggplot object
    expect_error(doc_plot(g, sponsor=g))
    
    # ggplot2 geom object
    expect_error(doc_plot(g, sponsor=geom_point()))
    
})

# ###################################################################################
# ensure error is thrown if logical object is not given for argument 'draw'
test_that("error thrown if logical is not given for argument 'draw'", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    # numeric
    expect_error(doc_plot(g, draw=3))
    
    # logical
    expect_error(doc_plot(g, draw="TRUE"))
    
    # vector with multiple character objects
    expect_error(doc_plot(g, draw=c("The scary internet!", "My local newspaper")))
    
    # data frame
    expect_error(doc_plot(g, draw=data.frame(x=1, y=2)))
    
    # data frame with data source as character in it
    expect_error(doc_plot(g, draw=data.frame(author="The scary internet!")))
    
    # ggplot object
    expect_error(doc_plot(g, draw=g))
    
    # ggplot2 geom object
    expect_error(doc_plot(g, draw=geom_point()))    
    
})

# ###################################################################################
# ensure valid arguments works properly!
test_that("no errors for valid arguments!", {
    
    # build a ggplot plot - required argument for doc_plot
    g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
    
    expect_is(doc_plot(g), c("gTree", "grob", "gDesc"))
    expect_is(doc_plot(g, draw=FALSE), c("gTree", "grob", "gDesc"))
    expect_is(doc_plot(g, 
                       img_sponsor=system.file("img", "Rlogo.png", package="png"),
                       author="Joshua Poirier",
                       author_title="Data Scientist",
                       data_source="My website",
                       theme="custom", bg="purple", fg="yellow"), c("gTree", "grob", "gDesc"))
})