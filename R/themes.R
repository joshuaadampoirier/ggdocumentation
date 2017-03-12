#' Retrieve a list of thematic elements
#' 
#' This function retrieves a list of thematic elements to be used by the
#' \link{doc_plot} function. The elements correspond to either a given theme
#' (such as from the \pkg{ggplot2} or \pkg{ggthemes} packages) or a custom theme
#' with relevant parameters also given.
#' 
#' @param theme \code{character}. Theme from the \pkg{ggplot2} or \pkg{ggthemes}
#'   packages. Defaults to 'gray'.
#' @inheritParams ggthemes::theme_economist_white
#' @inheritParams ggthemes::theme_hc
#' @inheritParams ggthemes::theme_pander
#' @inheritParams ggthemes::theme_solarized
#' @inheritParams ggthemes::theme_stata
#' @inheritParams ggthemes::theme_tufte
#' @inheritParams ggthemes::theme_wsj
#' @param bg \code{character}. Background color for documentation.
#' @param fg \code{character}. Foreground/font color.
#' @param cex \code{numeric}. Font size multiplier.
#' @param fill \code{character}. Background color for plot.
#' @param col \code{character}. Border color.
#' @param lwd \code{numeric}. Border line width.
#' @param lty \code{character}, \code{numeric}. Border line type. Accepts 
#'   numbers 0, 1, 2, 3, 4, 5, 6 or characters "blank", "solid", "dashed",
#'   "dotted", "dotdash", "longdash", or "twodash".
get_theme <- function(
                      # default theme
                      theme="gray",
                      
                      # standard theme parameters
                      base_size=NA,
                      base_family=NA,
                      
                      # economist, economist_white options (horizontal also used in excel theme)
                      horizontal=NA,
                      dkpanel=NA,
                      stata=NA,
                      gray_bg=NA,
                      
                      # hc theme option
                      bgcolor=NA,
                      
                      # pander theme options
                      nomargin=NA,
                      ff=NA,
                      fc=NA,
                      fs=NA,
                      gM=NA,
                      gm=NA,
                      gc=NA,
                      gl=NA,
                      boxes=NA,
                      bc=NA,
                      pc=NA,
                      lp=NA,
                      axis=NA,
                      
                      # solarized theme option
                      light=NA,
                      
                      # stata theme option
                      scheme=NA,
                      
                      # tufte theme option
                      ticks=NA,
                      
                      # wsj theme options
                      color=NA,
                      title_family=NA,
                      
                      # custom theme parameters
                      bg=NA,
                      fg=NA,
                      cex=NA,
                      fill=NA,
                      col=NA,
                      lwd=NA,
                      lty=NA
                      ) {
    
    # initialize list of relevant theme data
    t <- list()
    
    if (theme == "gray" | theme == "grey") {
        # ###################################################################################
        # theme_gray() or theme_grey() from the ggplot2 package

        theme <- theme_grey(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$strip.text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      "white"
        t[["lty"]] <-       "blank"
        
    } else if (theme == "bw") {
        # ###################################################################################
        # theme_bw() from the ggplot2 package

        theme <- theme_bw(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$panel.border$colour
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "linedraw") {
        # ###################################################################################
        # theme_linedraw() from the ggplot2 package

        theme <- theme_linedraw(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$strip.text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$panel.border$colour
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "light") {
        # ###################################################################################
        # theme_light() from the ggplot2 package

        theme <- theme_light(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$strip.text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "dark") {
        # ###################################################################################
        # theme_dark() from the ggplot2 package

        theme <- theme_dark(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$strip.text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"        
        
    } else if (theme == "minimal") {
        # ###################################################################################
        # theme_minimal() from the ggplot2 package

        theme <- theme_minimal(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$rect$fill
        t[["fg"]] <-        theme$strip.text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "classic") {
        # ###################################################################################
        # theme_classic() from the ggplot2 package

        theme <- theme_classic(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$strip.text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$strip.background$colour
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "void") {
        # ###################################################################################
        # theme_void() from the ggplot2 package

        theme <- theme_void(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        "white"
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      "white"
        t[["lty"]] <-       "blank"
        
    } else if (theme == "base") {
        # ###################################################################################
        # theme_base() from the ggthemes package

        # 'base' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
            
        
        theme <- theme_base(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        "white"
        t[["fg"]] <-        "black"
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      "white"
        t[["col"]] <-       "black"
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "calc") {
        # ###################################################################################
        # theme_calc() from the ggthemes package

        # 'calc' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_calc(base_size=base_size, base_family=base_family)
        
        # documentation display parameters
        t[["bg"]] <-        "white"
        t[["fg"]] <-        "black"
        t[["size"]] <-      ifelse(is.na(base_size), 10, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      "white"
        t[["col"]] <-       "gray70"
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "economist") {
        # ###################################################################################
        # theme_economist() from the ggthemes package

        # 'economist' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_economist(base_size   = ifelse(is.na(base_size), 11, base_size), 
                                 base_family = ifelse(is.na(base_family), "sans", base_family),
                                 horizontal  = ifelse(is.na(horizontal), TRUE, horizontal),
                                 dkpanel     = ifelse(is.na(dkpanel), FALSE, dkpanel),
                                 stata       = ifelse(is.na(stata), FALSE, stata))
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "economist_white") {
        # ###################################################################################
        # theme_economist_white() from the ggthemes package
        
        # 'economist_white' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_economist_white(base_size   = ifelse(is.na(base_size), 11, base_size),
                                       base_family = ifelse(is.na(base_family), "sans", base_family),
                                       gray_bg     = ifelse(is.na(gray_bg), TRUE, gray_bg),
                                       horizontal  = ifelse(is.na(horizontal), TRUE, horizontal))
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "excel") {
        # ###################################################################################
        # theme_excel() from the ggthemes package
        
        # 'excel' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_excel(base_size   = ifelse(is.na(base_size), 12, base_size), 
                             base_family = ifelse(is.na(base_family), "", base_family),
                             horizontal  = ifelse(is.na(horizontal), TRUE, horizontal))
        
        # documentation display parameters
        t[["bg"]] <-        theme$panel.background$fill
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$text$colour
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "few") {
        # ###################################################################################
        # theme_few() from the ggthemes package

        # 'few' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_few(base_size   = ifelse(is.na(base_size), 12, base_size),
                           base_family = ifelse(is.na(base_family), "", base_family))
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$panel.border$colour
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "fivethirtyeight") {
        # ###################################################################################
        # theme_fivethirtyeight() from the ggthemes package
  
        # 'fivethirtyeight' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }

        theme <- theme_fivethirtyeight(base_size   = ifelse(is.na(base_size), 12, base_size),
                                       base_family = ifelse(is.na(base_family), "sans", base_family))
        
        # documentation display parameters
        t[["bg"]] <-        theme$text$colour
        t[["fg"]] <-        theme$rect$fill
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "gdocs") {
        # ###################################################################################
        # theme_gdocs() from the ggthemes package

        # 'gdocs' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_gdocs(base_size   = ifelse(is.na(base_size), 12, base_size),
                             base_family = ifelse(is.na(base_family), "sans", base_family))
        
        # documentation display parameters
        t[["bg"]] <-        theme$rect$fill
        t[["fg"]] <-        theme$text$colour
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$rect$colour
        t[["lwd"]] <-       1
        t[["lty"]] <-       "solid"
        
    } else if (theme == "hc") {
        # ###################################################################################
        # theme_hc() from the ggthemes package

        # 'hc' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }

        theme <- theme_hc(base_size   = ifelse(is.na(base_size), 12, base_size),
                          base_family = ifelse(is.na(base_family), "sans", base_family),
                          bgcolor     = ifelse(is.na(bgcolor), "default", bgcolor))
        
        # documentation display parameters
        t[["bg"]] <-        theme$panel.grid.major.y$colour
        t[["fg"]] <-        "black"
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "pander") {
        # ###################################################################################
        # theme_pander() from the ggthemes package

        # 'pander' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_pander(base_size   = ifelse(is.na(base_size), 12, base_size),
                              base_family = ifelse(is.na(base_family), "sans", base_family),
                              nomargin    = ifelse(is.na(nomargin), TRUE, nomargin),
                              fc          = ifelse(is.na(fc), "black", fc),
                              gM          = ifelse(is.na(gM), TRUE, gM),
                              gm          = ifelse(is.na(gm), TRUE, gm),
                              gc          = ifelse(is.na(gc), "grey", gc),
                              gl          = ifelse(is.na(gl), "dashed", gl),
                              boxes       = ifelse(is.na(boxes), FALSE, boxes),
                              bc          = ifelse(is.na(bc), "white", bc),
                              pc          = ifelse(is.na(pc), "transparent", pc),
                              lp          = ifelse(is.na(lp), "right", lp),
                              axis        = ifelse(is.na(axis), 1, axis))
        
        # documentation display parameters
        t[["bg"]] <-        ifelse(is.na(gc), "grey", gc)
        t[["fg"]] <-        ifelse(is.na(fc), "black", fc)
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      ifelse(is.na(bc), "white", bc)
        t[["lty"]] <-       "blank"
        
    } else if (theme == "solarized") {
        # ###################################################################################
        # theme_solarized() from the ggthemes package

        # 'solarized' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        light <- ifelse(is.na(light), TRUE, light)
        
        theme <- theme_solarized(base_size     = ifelse(is.na(base_size), 12, base_size),
                                 base_family   = ifelse(is.na(base_family), "sans", base_family),
                                 light         = light)

        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$strip.text$col
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       ifelse(light, theme$strip.background$col, NA)
        t[["lwd"]] <-       ifelse(light, 1, 0)
        t[["lty"]] <-       ifelse(light, "solid", "blank")
    
    } else if (theme == "solarized_2" | theme == "solarized2") {
        # ###################################################################################
        # theme_solarized_2() from the ggthemes package
        
        # 'solarized_2' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }      
        
        light <- ifelse(is.na(light), TRUE, light)
        
        theme <- theme_solarized_2(base_size   = ifelse(is.na(base_size), 12, base_size),
                                   base_family = ifelse(is.na(base_family), "sans", base_family),
                                   light       = light)
        
        # documentation display parameters
        t[["bg"]] <-        ifelse(light, theme$panel.background$fill, theme$panel.background$fill)
        t[["fg"]] <-        theme$text$col
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
     
    } else if (theme == "stata") {
        # ###################################################################################
        # theme_stata() from the ggthemes package

        # 'stata' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_stata(base_size   = ifelse(is.na(base_size), 11, base_size),
                             base_family = ifelse(is.na(base_family), "sans", base_family),
                             scheme      = ifelse(is.na(scheme), "s2color", scheme))
        
        # documentation display parameters
        t[["bg"]] <-        theme$strip.background$fill
        t[["fg"]] <-        theme$text$col
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "sans", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
    
    } else if (theme == "tufte") {
        # ###################################################################################
        # theme_tufte() from the ggthemes package

        # 'tufte' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }

        theme <- theme_tufte(base_size   = ifelse(is.na(base_size), 11, base_size),
                             base_family = ifelse(is.na(base_family), "serif", base_family),
                             ticks       = ifelse(is.na(ticks), TRUE, ticks))
        
        # documentation display parameters
        t[["bg"]] <-        theme$rect$fill
        t[["fg"]] <-        theme$text$col
        t[["size"]] <-      ifelse(is.na(base_size), 11, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), "serif", base_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["lty"]] <-       "blank"
        
    } else if (theme == "wsj") {
        # ###################################################################################
        # theme_wsj() from the ggthemes package

        # 'wsj' theme requires ggthemes package
        if (!"package:ggthemes" %in% search()) {
            stop(paste0("Error! ggthemes package not loaded - it is required for the '", theme, "' theme."))
        }
        
        theme <- theme_wsj(base_size     = ifelse(is.na(base_size), 12, base_size),
                           color         = ifelse(is.na(color), "brown", color),
                           base_family   = ifelse(is.na(base_family), "sans", base_family),
                           title_family  = ifelse(is.na(title_family), "mono", title_family))
        
        # documentation display parameters
        t[["bg"]] <-        theme$rect$fill
        t[["fg"]] <-        theme$text$col
        t[["size"]] <-      ifelse(is.na(base_size), 12, base_size)
        t[["family"]] <-    ifelse(is.na(title_family), "mono", title_family)
        t[["cex"]] <-       .75
        
        # plot display parameters
        t[["fill"]] <-      theme$rect$fill
        t[["col"]] <-       theme$panel.grid.major$col
        t[["lwd"]] <-       1
        t[["lty"]] <-       "dotted"
     
    } else if (theme == "custom") {
        # ###################################################################################
        # build a custom theme using passed arguments    
        
        # use default to fill any missing arguments
        grey <- get_theme()   
        
        # documentation display parameters
        t[["bg"]] <-        ifelse(is.na(bg), grey$bg, bg)
        t[["fg"]] <-        ifelse(is.na(fg), grey$fg, fg)
        t[["size"]] <-      ifelse(is.na(base_size), grey$size, base_size)
        t[["family"]] <-    ifelse(is.na(base_family), grey$family, base_family)
        t[["cex"]] <-       ifelse(is.na(cex), grey$cex, cex)
        
        # plot display parameters
        t[["fill"]] <-      ifelse(is.na(fill), grey$fill, fill)
        if (!is.na(col)) { t[["col"]] <- col }
        if (!is.na(lwd)) { t[["lwd"]] <- lwd }
        t[["lty"]] <-       ifelse(is.na(lty), grey$lty, lty)
        
        t <- validate_custom_args(t)
        
    } else {
        stop(paste0("Error! Theme '", theme, "' not found."))
    }
    
    t
}

# validate if a given string "col" is a valid color
isColor <- function(col) {
    t <- try(col2rgb(col), silent=TRUE)
    r <- !"try-error" %in% class(t)
    r
}

# validate arguments for custom theme
validate_custom_args <- function(t) {

    def <- get_theme()
    
    # validate 'bg' - background color
    if(!isColor(t$bg)) {
        warning(paste("Warning! Color", t$bg, "is not valid - proceeding with default bg color."))
        t$bg <- def$bg
    }
    
    # validate 'fg' - foreground color
    if(!isColor(t$fg)) {
        warning(paste("Warning! Color", t$fg, "is not valid - proceeding with default fg color."))
        t$fg <- def$fg
    }
    
    # validate 'size' - text font size
    if(!is.numeric(t$size) | (is.numeric(t$size) & t$size <= 0)) {
        warning(paste("Warning! size", t$size, "is not valid - proceeding with default size."))
        t$size <- def$size
        
    }
    
    # validate 'family' - documentation font family (only support serif, sans, mono, and symbol)
    if(!t$family %in% c("", "serif", "sans", "mono", "symbol")) {
        warning(paste("Warning! family", t$family, "is not valid or not supported - proceeding with default font family."))
        t$family <- def$family
    }
    
    # validate 'cex' - documentation font size multiplier
    if(!is.numeric(t$cex) | (is.numeric(t$cex) & t$cex <= 0)) {
        warning(paste("Warning! cex", t$cex, "is not valid - proceeding with default size."))
        t$cex <- def$cex
    }
    
    # validate 'fill' - plot fill color
    if(!isColor(t$fill)) {
        warning(paste("Warning! Color", t$fill, "is not valid - proceeding with default fill color."))
        t$fill <- def$fill
    }
    
    # validate 'col' - plot border color
    if(!class(t$col) == "NULL") {
        if(!isColor(t$col)) {
            warning(paste("Warning! col", t$col, "is not valid - proceeding with default border color."))
            t$col <- "black"
        }
    }
    
    # validate 'lwd' - plot border line width
    if(!class(t$lwd) == "NULL") {
        if(!is.numeric(t$lwd) | (is.numeric(t$lwd) & t$lwd <= 0)) {
            warning(paste("Warning! lwd", t$lwd, "is not valid - proceeding with default line width."))
            t$lwd <- 1
        }
    }
    
    # validate 'lty' - plot border line type
    if(!t$lty %in% c(0, 1, 2, 3, 4, 5, 6, "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) {
        warning(paste("Warning! lty", t$lty, "is not valid - proceeding with default plot border line type."))
        t$lty <- def$lty
    }
    
    t
}