library(png)
library(grid)
library(ggplot2)

getTheme <- function(theme="economist") {
    
    if (theme == "base") {
        # a theme resembling the default base graphics in R    
        cols <- c("white", "white", "black", TRUE, "black")
    } else if (theme == "calc") {
        # a theme based on LibreOffice Calc
        cols <- c("white", "white", "black", TRUE, "black")
    } else if (theme == "economist") {
        # a theme based on the plots in The Economist newspaper
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "economist_white") {
        # ditto
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "excel") {
        # a theme replicating classic charts in Excel
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "few") {
        # theme from Stephen Few's 'Practical Rules for Using Color in Charts'
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "fivethirtyeight") {
        # theme based on the plots at fivethirtyeight.com
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "gdocs") {
        # theme based on Google Docs
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "hc") {
        # theme based on Highcharts JS
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "par") {
        # theme that uses the current values of the base graphics parameters in par
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "pander") {
        # theme to use with the pander package
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "solarized") {
        # theme using the solarized color palette
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "stata") {
        # themes based on Stata graph schemes
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "tufte") {
        # a minimal ink theme based on Tufte's The Visual Display of Quantitative Information
        cols <- c("white", "grey", "black", TRUE, "black")
    } else if (theme == "wsj") {
        # a theme based on the plots from The Wall Street Journal
        cols <- c("white", "grey", "black", TRUE, "black")
    }
    
    names(cols) <- c("bg", "docbg", "fg", "addrect", "rectcol")
    cols
}

doc_plot <- function(d, fname, aut="", aut_tit="", dsrc="", theme="") {

    if (theme != "") {
        cols <- getTheme(theme)
    } else {
        cols <- c("white", "grey", "black", TRUE, "black")
        names(cols) <- c("bg", "docbg", "fg", "addrect", "rectcol")
    }
    
    # Set up the layout for grid 
    lo = grid.layout(2, 7, widths=unit(c(.01, .1, .01, .43, .01, .43, .01), "npc"), heights=unit(c(.93, .07),"npc"))
    grid.show.layout(lo)
    grid.newpage()
    pushViewport(viewport(layout = lo))

    # background color of plot
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    print(grid.draw(rectGrob(gp=gpar(fill=cols["bg"], col=ifelse(cols["addrect"], cols["rectcol"], cols["bg"])))), newpage=FALSE)
    popViewport()
    
    # background color of documentation
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:7))
    print(grid.draw(rectGrob(gp=gpar(fill=cols["docbg"], col=ifelse(cols["addrect"], cols["rectcol"], cols["docbg"])))), newpage=FALSE)
    popViewport()
        
    # add plot to upper grid object
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    if (cols["addrect"]) {
        #d$theme$rect[["colour"]] <- "white"
        print(d$theme$rect[["colour"]])
    }
    print(d, newpage=FALSE)
    popViewport()
    
    colWidths <- NULL
    for(i in 1:7) {
        pushViewport(viewport(layout.pos.row=1, layout.pos.col=i))
        o <- rectGrob(gp=gpar(col="black", fill="#d5e4eb"))
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
                             gp=gpar(col=cols["fg"], cex=.8))

        print(grid.draw(author_o), newpage=FALSE)
        popViewport()        
    }

    # The data source
    if (dsrc != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=6))
        source_o <- textGrob(paste("Source:", dsrc, "\n", format(Sys.Date(), "%d %b %Y")), just=1,
                             x=unit(sum(colWidths[1:6])/sum(colWidths), "npc"),
                             gp=gpar(col=cols["fg"], cex=.8))
        print(grid.draw(source_o), newpage=FALSE)
        popViewport()        
    }
    
    popViewport()
    
    # To save the object
    g = grid.grab()
    
    grid.newpage()
    grid.draw(g)
}

library(ggthemes)

# Get the graph
d <- ggplot(diamonds, aes(carat, price)) + theme_calc() +
    xlim(0, 2) + 
    stat_binhex(na.rm = TRUE) + 
    theme(aspect.ratio = 1, legend.position="right") + 
    facet_wrap(~ color, scales = "free_x")
doc_plot(d, 
         fname=system.file("img", "Rlogo.png", package="png"), 
         aut="Joshua Poirier", 
         aut_tit="Data Scientist", 
         dsrc="My website",
         theme="calc")