library(png)
library(grid)
library(ggplot2)

get_ggtheme <- function(theme="economist") {
    # initialize list of 
    t <- list()
    
    if (theme == "base") {

        try ((if (!"package:ggthemes" %in% search()) 
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .5
        t[["theme"]] <- theme_base() + theme(rect=element_blank(), strip.background=element_rect(fill="white"))
    } else if (theme == "calc") {
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "gray70"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .75
        t[["theme"]] <- theme_calc() + theme(rect=element_blank(), strip.background=element_rect(color="black"))

    } else if (theme == "economist") {
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- ggthemes_data$economist$bg["ebg"]
        t[["docbg"]] <- ggthemes_data$economist$bg["edkbg"]
        t[["fg"]] <- ggthemes_data$economist$fg["blue_gray"]
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_economist()
        
    } else if (theme == "economist_white") {
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- ggthemes_data$economist$bg["ltgray"]
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_economist_white()
        
    } else if (theme == "excel") {
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "#C0C0C0"
        t[["fg"]] <- "white"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .75
        t[["theme"]] <- theme_excel()
        
    } else if (theme == "few") {

        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_few()
        
    } else if (theme == "fivethirtyeight") {
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- ggthemes_data$fivethirtyeight["ltgray"]
        t[["docbg"]] <- ggthemes_data$fivethirtyeight["dkgray"]
        t[["fg"]] <- ggthemes_data$fivethirtyeight["ltgray"]
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_fivethirtyeight()
        
    } else if (theme == "gdocs") {

        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "#CCCCCC"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .75
        t[["theme"]] <- theme_gdocs() + theme(rect=element_blank(), strip.background=element_rect(color="black"))
        
    } else if (theme == "hc") {

        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- ggthemes_data$hc$bg["darkunica"]
        t[["fg"]] <- "white"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_hc() 
        
    } else if (theme == "pander") {
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "grey"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_pander() + theme(text=element_text(size=12))
        
    } else if (theme == "solarized") {
 
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "#002b36"
        t[["docbg"]] <- "grey85"
        t[["fg"]] <- "#073642"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_solarized()
        
    } else if (theme == "stata") {

        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- ggthemes_data$stata$colors["ltbluishgray"]
        t[["docbg"]] <- ggthemes_data$stata$colors["bluishgray"]
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_stata()
        
    } else if (theme == "tufte") {

        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_tufte()
        
    } else if (theme == "wsj") {

        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded.")))
        
        t[["bg"]] <- ggthemes_data$wsj$bg["brown"]
        t[["docbg"]] <- ggthemes_data$wsj$bg["brown"]
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_wsj()
        
    }
    
    t
}

doc_plot <- function(d, fname, aut="", aut_tit="", dsrc="", theme="") {

    if (theme != "") {
        t <- get_ggtheme(theme)
    } else {
        t <- list()
        t[["bg"]] <- "white"
        t[["docbg"]] <- "grey"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- 1
        t[["theme"]] <- theme(
            rect=element_blank(), 
            strip.background=element_rect(fill="grey"),
            text=element_text(color="black", face="plain", size=16, vjust=.5, hjust=.5, lineheight=1)
            )
    }
    
    # Set up the layout for grid 
    lo = grid.layout(2, 7, widths=unit(c(.01, .1, .01, .43, .01, .43, .01), "npc"), heights=unit(c(.93, .07),"npc"))
    grid.show.layout(lo)
    grid.newpage()
    pushViewport(viewport(layout = lo))

    # background color of plot
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=t$bg))), newpage=FALSE)
    popViewport()
    
    # background color of documentation
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=t$docbg))), newpage=FALSE)
    popViewport()
        
    # add plot to upper grid object
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    d <- d + t$theme
    print(d, newpage=FALSE)
    popViewport()
    
    colWidths <- NULL
    for(i in 1:7) {
        pushViewport(viewport(layout.pos.row=1, layout.pos.col=i))
        o <- rectGrob()
        colWidths <- c(colWidths, widthDetails(o))
        popViewport()
    }

    # The logo
    img <- readPNG(fname)
    img_o <- rasterGrob(img, height=unit(0.8, "npc"))
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    print(grid.draw(img_o), newpage=FALSE)
    popViewport()
    
    # The author
    if (aut != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=4))
        author_o <- textGrob(paste(aut, "\n", aut_tit), just=0,
                             x=unit(sum(colWidths[1:3])/sum(colWidths), "npc"), 
                             gp=gpar(col=t$fg, fontfamily=t$theme$text$family, fontsize=t$cex*t$theme$text$size))

        print(grid.draw(author_o), newpage=FALSE)
        popViewport()        
    }

    # The data source
    if (dsrc != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=6))
        source_o <- textGrob(paste("Source:", dsrc, "\n", format(Sys.Date(), "%d %b %Y")), just=1,
                             x=unit(sum(colWidths[1:6])/sum(colWidths), "npc"),
                             gp=gpar(col=t$fg, fontfamily=t$theme$text$family, fontsize=t$cex*t$theme$text$size))
        print(grid.draw(source_o), newpage=FALSE)
        popViewport()        
    }
    
    # documentation border (if applicable)
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=NA, col=ifelse(t$addrect, t$rectcol, t$docbg)))), newpage=FALSE)
    popViewport()
    
    # plot border (if applicable)
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=NA, col=ifelse(t$addrect, t$rectcol, t$docbg)))), newpage=FALSE)
    popViewport()
    
    popViewport()
    
    # To save the object
    g = grid.grab()
    
    grid.newpage()
    grid.draw(g)
}

library(ggthemes)

# Get the graph
d <- ggplot(diamonds, aes(carat, price)) + theme_wsj() +
    xlim(0, 2) + 
    stat_binhex(na.rm = TRUE) + 
    theme(aspect.ratio = 1, legend.position="right") + 
    facet_wrap(~ color, scales = "free_x")
#d
doc_plot(d,
        fname=system.file("img", "Rlogo.png", package="png"),
        aut="Joshua Poirier",
        aut_tit="Data Scientist",
        dsrc="My website",
        theme="wsj")