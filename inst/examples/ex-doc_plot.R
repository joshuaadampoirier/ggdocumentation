library("ggplot2")
library("ggthemes")
library("ggdocumentation")

# Use doc_plot to add documentation to a plot in fivethirtyeight styling
# ##############################################################################

# build ggplot2 object
g <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
    labs(title="Iris data using 'ggdocumentation'!") +
    scale_color_fivethirtyeight() +
    theme_fivethirtyeight() +
    geom_point()

# prep graphics device to save as png file
png(filename="figures/iris_fivethirtyeight.png", 
    width=1100, height=800, units="px")

# add the documentation
d <- doc_plot(g, 
         author="Joshua Poirier", 
         data_source="Source: Fisher, 1936", 
         img_sponsor="figures/mvp-logo.png",
         theme="fivethirtyeight",
         base_size=16)

# save to file!
print(d)
dev.off()

# Use doc_plot to add documentation to a plot using custom styling
# ##############################################################################

# build a ggplot2 object 
g <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
    labs(title="Iris data using 'ggdocumentation'!") +
    geom_point() + theme_grey() +
    theme(rect=element_rect(fill="white"),
          text=element_text(color="black"))

# prep graphics device to save as png file
# prep graphics device to save as png file
png(filename="figures/iris_custom.png", 
    width=1100, height=800, units="px")

# add the documentation
d <- doc_plot(g, 
              author="Joshua Poirier", 
              author_title="Data Scientist",
              data_source="Source: Fisher, 1936", 
              date=TRUE,
              img_sponsor="figures/mvp-logo.png",
              theme="custom",
              base_size=14,
              bg="dodgerblue4",
              fg="white",
              fill="white",
              col="dodgerblue4",
              lty="solid")

# save to file!
print(d)
dev.off()

# Use doc_plot to add documentation to a plot combining stylings
# ##############################################################################

# build ggplot2 object
g <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
    labs(title="Iris data using 'ggdocumentation'!") +
    theme_solarized() +
    geom_point()

# prep graphics device to save as png file
png(filename="figures/iris_combo.png", 
    width=1100, height=800, units="px")

# add the documentation
d <- doc_plot(g, 
              author="Joshua Poirier", 
              data_source="Source: Fisher, 1936", 
              img_sponsor="figures/mvp-logo.png",
              theme="economist",
              base_size=16)

# save to file!
print(d)
dev.off()

