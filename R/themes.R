get_theme <- function(theme="gray") {
    # initialize list of relevant theme data
    t <- list()
    
    if (theme == "base") {
        # theme_base() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search()) 
            stop("Error! ggthemes package not loaded - it is required for the 'base' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .5
        t[["theme"]] <- theme_base() + theme(rect=element_blank(), strip.background=element_rect(fill="white"))
    } else if (theme == "calc") {
        # theme_calc() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'calc' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "gray70"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .75
        t[["theme"]] <- theme_calc() + theme(rect=element_blank(), strip.background=element_rect(color="black"))
        
    } else if (theme == "economist") {
        # theme_economist() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'economist' theme.")))
        
        t[["bg"]] <- ggthemes_data$economist$bg["ebg"]
        t[["docbg"]] <- ggthemes_data$economist$bg["edkbg"]
        t[["fg"]] <- ggthemes_data$economist$fg["blue_gray"]
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_economist()
        
    } else if (theme == "economist_white") {
        # theme_economist_white() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'economist_white' theme.")))
        
        t[["bg"]] <- ggthemes_data$economist$bg["ltgray"]
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_economist_white()
        
    } else if (theme == "excel") {
        # theme_excel() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'excel' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "#C0C0C0"
        t[["fg"]] <- "white"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .75
        t[["theme"]] <- theme_excel()
        
    } else if (theme == "few") {
        # theme_few() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'few' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_few()
        
    } else if (theme == "fivethirtyeight") {
        # theme_fivethirtyeight() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'fivethirtyeight' theme.")))
        
        t[["bg"]] <- ggthemes_data$fivethirtyeight["ltgray"]
        t[["docbg"]] <- ggthemes_data$fivethirtyeight["dkgray"]
        t[["fg"]] <- ggthemes_data$fivethirtyeight["ltgray"]
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_fivethirtyeight()
        
    } else if (theme == "gdocs") {
        # theme_gdocs() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'gdocs' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "#CCCCCC"
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- "black"
        t[["cex"]] <- .75
        t[["theme"]] <- theme_gdocs() + theme(rect=element_blank(), strip.background=element_rect(color="black"))
        
    } else if (theme == "hc") {
        # theme_hc() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'hc' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- ggthemes_data$hc$bg["darkunica"]
        t[["fg"]] <- "white"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_hc() 
        
    } else if (theme == "pander") {
        # theme_pander() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'pander' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "grey"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_pander() + theme(text=element_text(size=12))
        
    } else if (theme == "solarized") {
        # theme_solarized() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'solarized' theme.")))
        
        t[["bg"]] <- "#002b36"
        t[["docbg"]] <- "grey85"
        t[["fg"]] <- "#073642"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_solarized()
        
    } else if (theme == "stata") {
        # theme_stata() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'stata' theme.")))
        
        t[["bg"]] <- ggthemes_data$stata$colors["ltbluishgray"]
        t[["docbg"]] <- ggthemes_data$stata$colors["bluishgray"]
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_stata()
        
    } else if (theme == "tufte") {
        # theme_tufte() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'tufte' theme.")))
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_tufte()
        
    } else if (theme == "wsj") {
        # theme_wsj() from the ggthemes package
        
        try ((if (!"package:ggthemes" %in% search())
            stop("Error! ggthemes package not loaded - it is required for the 'wsj' theme.")))
        
        t[["bg"]] <- ggthemes_data$wsj$bg["brown"]
        t[["docbg"]] <- ggthemes_data$wsj$bg["brown"]
        t[["fg"]] <- "black"
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_wsj()
        
    } else if (theme == "gray" | theme == "grey") {
        # theme_gray() or theme_grey() from the ggplot2 package
        
        
        t[["bg"]] <- theme_grey()$rect$fill
        t[["docbg"]] <- theme_grey()$strip.background$fill
        t[["fg"]] <- theme_grey()$text$color
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_grey()
        
    } else if (theme == "bw") {
        # theme_bw() from the ggplot2 package
        
        t[["bg"]] <- theme_bw()$rect$fill
        t[["docbg"]] <- theme_bw()$strip.background$fill
        t[["fg"]] <- theme_bw()$text$color
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- theme_bw()$panel.border$colour
        t[["cex"]] <- .75
        t[["theme"]] <- theme_bw()
        
    } else if (theme == "linedraw") {
        # theme_linedraw() from the ggplot2 package
        
        t[["bg"]] <- theme_linedraw()$rect$fill
        t[["docbg"]] <- theme_linedraw()$strip.background$fill
        t[["fg"]] <- theme_linedraw()$strip.text$colour
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- theme_linedraw()$panel.border$colour
        t[["cex"]] <- .75
        t[["theme"]] <- theme_linedraw()
        
    } else if (theme == "light") {
        # theme_light() from the ggplot2 package
        
        t[["bg"]] <- theme_light()$rect$fill
        t[["docbg"]] <- theme_light()$strip.background$fill
        t[["fg"]] <- theme_light()$strip.text$colour
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_light()
        
    } else if (theme == "dark") {
        # theme_dark() from the ggplot2 package
        
        t[["bg"]] <- theme_dark()$rect$fill
        t[["docbg"]] <- theme_dark()$strip.background$fill
        t[["fg"]] <- theme_dark()$strip.text$colour
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_dark()
        
    } else if (theme == "minimal") {
        # theme_minimal() from the ggplot2 package
        
        t[["bg"]] <- theme_minimal()$rect$fill
        t[["docbg"]] <- theme_minimal()$rect$fill
        t[["fg"]] <- theme_minimal()$strip.text$colour
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_minimal()
        
    } else if (theme == "classic") {
        # theme_classic() from the ggplot2 package
        
        t[["bg"]] <- theme_classic()$rect$fill
        t[["docbg"]] <- theme_classic()$strip.background$fill
        t[["fg"]] <- theme_classic()$strip.text$colour
        t[["addrect"]] <- TRUE
        t[["rectcol"]] <- theme_classic()$strip.background$colour
        t[["cex"]] <- .75
        t[["theme"]] <- theme_classic()
        
    } else if (theme == "void") {
        # theme_void() from the ggplot2 package
        
        t[["bg"]] <- "white"
        t[["docbg"]] <- "white"
        t[["fg"]] <- theme_void()$text$colour
        t[["addrect"]] <- FALSE
        t[["cex"]] <- .75
        t[["theme"]] <- theme_void()
        
    } else {
        stop(paste0("Error! Theme '", theme, "' not found."))
    }
    
    t
}