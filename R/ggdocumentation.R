library(png)
library(grid)
library(ggplot2)

source("themes.R")

doc_plot <- function(d, fname, aut="", aut_tit="", dsrc="", theme="gray", ...) {

    t <- get_theme(theme, ...)

    # Set up the layout for grid 
    w <- c(.01, .1, .01, .43, .01, .43, .01)
    h <- c(.93, .07)
    lo = grid.layout(2, 7, widths=unit(w, "npc"), heights=unit(h,"npc"))
    grid.show.layout(lo)
    grid.newpage()
    pushViewport(viewport(layout = lo))

    # background color of plot
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=t$fill, col=t$fill))), newpage=FALSE)
    popViewport()
    
    # remove plot borders and add plot to upper grid object
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:7))
    #d <- d + theme(rect=element_rect(color=NA))
    if (t$lty != "blank") {
        d <- d + theme(rect=element_rect(color=NA))
    }
    print(d, newpage=FALSE)
    popViewport()
    
    # background color of documentation
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:7))
    print(grid.draw(rectGrob(width=unit(.999, "npc"), 
                             height=unit(.999, "npc"), 
                             gp=gpar(fill=t$bg, col=t$bg))), newpage=FALSE)
    popViewport()
        
    # sponsor logo
    img <- readPNG(fname)
    img_o <- rasterGrob(img, height=unit(0.8, "npc"))
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    print(grid.draw(img_o), newpage=FALSE)
    popViewport()
    
    # aut = who did the work (eg. Charles Minard)
    # aut_tit = who's that? (eg. Inspector General of Bridges and Roads in retirement)
    if (aut != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=4))
        author_o <- textGrob(paste0(aut, "\n", aut_tit), just=0,
                             x=unit(sum(w[1:3]), "npc"),
                             gp=gpar(col=t$fg, fontfamily=t$family, fontsize=t$cex*t$size))

        print(grid.draw(author_o), newpage=FALSE)
        popViewport()        
    }

    # dsrc = what are the data sources (eg. M. M. Thiers and Jacob)
    # when was the work done? (eg. 20 Nov 1869)
    if (dsrc != "") {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col=6))
        source_o <- textGrob(paste0("Source:", dsrc, "\n", format(Sys.Date(), "%d %b %Y")), just=1,
                             x=unit(sum(w[1:6]), "npc"),
                             gp=gpar(col=t$fg, fontfamily=t$family, fontsize=t$cex*t$size))
        print(grid.draw(source_o), newpage=FALSE)
        popViewport()        
    }
    
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
    g = grid.grab()
    
    grid.newpage()
    grid.draw(g)
}

library(ggthemes)

# Get the graph
d <- ggplot(diamonds, aes(carat, price)) + theme_fivethirtyeight() +
    xlim(0, 2) + 
    stat_binhex(na.rm = TRUE) + 
    facet_wrap(~ color, scales = "free_x")
#d
doc_plot(d,
        fname=system.file("img", "Rlogo.png", package="png"),
        aut="Joshua Poirier",
        aut_tit="Data Scientist",
        dsrc="My website",
        theme="custom", bg="purple", fg="yellow")