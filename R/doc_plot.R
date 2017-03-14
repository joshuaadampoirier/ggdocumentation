#' Add documentation to ggplot2 plots
#' 
#' Adds documentation elements such as sponsor, author, author job title, and
#' data sources to annotate ggplot2 plots. Documentation is one of the 
#' principles of analytical design according to Edward Tufte's "Beautiful 
#' Evidence".
#' 
#' @param g A ggplot2 object to which documentation is to be added.
#' @param author A character object indicating the author of the plot.
#' @param author_title A character object indicating the job title of the author
#'   of the plot.
#' @param data_source A character object indicating the sources of the data 
#'   displayed in the plot.
#' @param date A logical indicating whether or not to add today's date as the 
#'   publishing date of the plot. It defaults to FALSE.
#' @param img_sponsor A character object representing the filename/filepath to
#'   a *.png image file representing the sponsor (ie. company logo)
#' @param sponsor A character object indicating the sponsor of the plot. Ignored
#'   if img_sponsor is used.
#' @param theme A character object indicating the theme for the documentation.
#'   This value should correspond to themes from the ggplot2 or ggthemes 
#'   packages. It defaults to 'gray'.
#' @param ... Additional parameters to be passed into the get_theme() function. 
#' @return The object to be plotted.
#' @export
#' @examples
#' library("grid")
#' library("ggplot2")
#' library("ggdocumentation")
#' 
#' # works with ggplot2 quick plots!
#' g <- qplot(Sepal.Length, Petal.Length, data=iris, color=Species)
#' 
#' # add documentation - automatically draws to canvas
#' doc_plot(g,
#'          author="Joshua Poirier",
#'          data_source="Fisher, 1936",
#'          sponsor="MVP")
#' 
#' # works with themes from ggthemes package!
#' library("ggthemes")
#' g <- ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length, col=Species)) +
#'      labs(title="Iris data using 'ggdocumentation'") +
#'      scale_fivethirtyeight() +
#'      theme_fivethirtyeight() +
#'      geom_point()
#'      
#' doc_plot(g, 
#'          author="Joshua Poirier",
#'          author_title="Data Scientist",
#'          data_source="Fisher, 1936",
#'          date=TRUE,
#'          img_sponsor="figures/mvp-logo.png",
#'          theme="fivethirtyeight")
#' 
#' # works with faceted plots too!
#' g <- ggplot(iris, aes(Sepal.Length, Petal.Length, col=Species)) +
#'      labs(title="Iris data using 'ggdocumentation'!") +
#'      facet_grid(. ~ Species) +
#'      scale_color_economist() +
#'      theme_economist(dkpanel=TRUE) +
#'      geom_point()
#' 
#' doc_plot(g,
#'          author="Joshua Poirier",
#'          data_source="Fisher, 1936",
#'          img_sponsor="figures/mvp-logo.png",
#'          theme="economist",
#'          dkpanel=TRUE)
doc_plot <- function(g, author="", author_title="", data_source="", date=FALSE, img_sponsor="", sponsor="", theme="gray", ...) {
    
    d <- ggplot(diamonds, aes(carat, price)) + 
        xlim(0, 2) + 
        stat_binhex(na.rm = TRUE) + 
        theme(aspect.ratio = 1, legend.position="right") + 
        facet_wrap(~ color, scales = "free_x")
    
    validate_args(g, author, author_title, data_source, date, img_sponsor, sponsor)
    
    t <- get_theme(theme, ...)
    
    # ###################################################################################
    # Set up the layout for grid 
    w <- c(.01, .1, .01, .43, .01, .43, .01)
    h <- c(.93, .07)
    lo = grid.layout(2, 7, widths=unit(w, "npc"), heights=unit(h,"npc"))
    grid.show.layout(lo)
    grid.newpage()
    pushViewport(viewport(layout = lo))
    
    # ###################################################################################
    # background color of plot
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=t$fill, col=t$fill))), newpage=FALSE)
    popViewport()
    
    # add plot to upper grid object
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    if (t$lty != "blank") {
        g <- g + theme(rect=element_rect(color=NA))
    }
    print(g, newpage=FALSE)
    popViewport()
    
    # ###################################################################################
    # background color of documentation
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=t$bg, col=t$bg))), newpage=FALSE)
    popViewport()
    
    # ###################################################################################    
    # sponsor (logo if 'img_sponsor' given; 'sponsor' as text otherwise)
    if (img_sponsor != "") {
        img_f <- readPNG(img_sponsor)
        
        if (dim(img_f)[2] > (2 * dim(img_f)[1])) {
            img_o <- rasterGrob(img_f, width=unit(0.8, "npc"))
        } else {
            img_o <- rasterGrob(img_f, height=unit(0.8, "npc"))
        }
        
             
    } else {
        img_o <- textGrob(sponsor, just=0,
                          x=unit(sum(w[1]), "npc"),
                          gp=gpar(col=t$fg, fontfamily=t$family, fontsize=t$cex*t$size))
    }
    
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    print(grid.draw(img_o), newpage=FALSE)
    popViewport()
    
    # ###################################################################################
    # aut = who did the work (eg. Charles Minard)
    # aut_tit = who's that? (eg. Inspector General of Bridges and Roads in retirement)
    if (author != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=4))
        
        message <- ifelse(author_title == "", 
                          author, 
                          paste0(author, "\n", author_title))
        
        author_o <- textGrob(message, just=0,
                             x=unit(sum(w[1:3]), "npc"),
                             gp=gpar(col=t$fg, fontfamily=t$family, fontsize=t$cex*t$size))
        
        print(grid.draw(author_o), newpage=FALSE)
        popViewport()        
    }
    
    # ###################################################################################
    # dsrc = what are the data sources (eg. M. M. Thiers and Jacob)
    # when was the work done? (eg. 20 Nov 1869)
    if (data_source != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=6))
        
        message <- ifelse(date, 
                          paste0(data_source, "\n", format(Sys.Date(), "%d %b %Y")), 
                          data_source)
        
        source_o <- textGrob(message, just=1,
                             x=unit(sum(w[1:6]), "npc"),
                             gp=gpar(col=t$fg, fontfamily=t$family, fontsize=t$cex*t$size))
        print(grid.draw(source_o), newpage=FALSE)
        popViewport()        
    }
    
    # ###################################################################################
    # borders
    if (!t$lty == "blank") {
        
        # documentation border
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:7))
        print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                                 height=unit(.999, "npc"), 
                                 gp=gpar(fill=NA, col=t$col, size=t$lwd, lty=t$lty))), newpage=FALSE)
        popViewport()
        
        # plot border
        pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
        print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                                 height=unit(.999, "npc"), 
                                 gp=gpar(fill=NA, col=t$col, size=t$lwd, lty=t$lty))), newpage=FALSE)
        popViewport()   
    }
    
    popViewport()
    
    # To save the object
    p = grid.grab()
    
    grid.newpage()
    grid.draw(p)
    
    p
}

# function to validate arguments going into 'doc_plot'
validate_args <- function(g, author, author_title, data_source, date, img_sponsor, sponsor) {
    
    # ensure argument 'g' is a ggplot object
    if (!(class(g)[1] == "gg" & class(g)[2] == "ggplot")) {
        stop(paste("Error! doc_plot argument 'g' must be a ggplot2 plot object!", class(g), "given."))
    }
    
    # ensure argument 'author' is class character
    if (class(author)[1] != "character" | length(author) > 1) {
        stop(paste("Error! doc_plot argument 'author' must be of length '1' and class 'character'!", class(author), "given."))
    }
    
    # ensure argument 'author_title' is class character
    if (author != "" & (class(author_title)[1] != "character" | length(author_title) > 1)) {
        stop(paste("Error! doc_plot argument 'author_title' must be of length '1' and class 'character'!", class(author_title), "given."))
    }
    
    # ensure argument 'data_source' is class character
    if (class(data_source)[1] != "character" | length(data_source) > 1) {
        stop(paste("Error! doc_plot argument 'data_source' must be of length '1' and class 'character'!", class(data_source), "given."))
    }
    
    # ensure argument 'date' is class logical
    if (class(date)[1] != "logical" | length(date) > 1) {
        stop(paste("Error! doc_plot argument 'date' must be of length '1' and class 'logical'!", class(date), "given."))
    }
    
    # ensure argument 'img_sponsor' is class character
    if (class(img_sponsor)[1] != "character" | length(img_sponsor) > 1) {
        stop(paste("Error! doc_plot argument 'img_sponsor' must be of length '1' and class 'character'!", class(img_sponsor), "given."))
    }
    
    # if no 'img_sponsor' is given, ensure 'sponsor' argument is of class 'character'
    if (img_sponsor == "" & (class(sponsor)[1] != "character" | length(sponsor) > 1)) {
        stop(paste("Error! doc_plot argument 'sponsor' must be of length '1' and class 'character'!", class(sponsor), "given"))
    } 
    
}