library(png)
library(grid)
library(ggplot2)

source("themes.R")

doc_plot <- function(d, fname, aut="", aut_tit="", dsrc="", theme="") {

    t <- get_theme(theme)

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
d <- ggplot(diamonds, aes(carat, price)) + theme_void() +
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
        theme="void")