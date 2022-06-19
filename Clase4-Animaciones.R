####
library(gapminder)
library(dplyr)
library(ggplot2)
# write.csv(gapminder, "gapminder.csv")
g<- list(scope = 'usa',
          projection = list(type = 'albers usa'),
          lakecolor = toRGB('white'))

density <- state.x77[, "Population"] / state.x77[, "Area"]

map <- plot_geo(z = ~density, text = state.name,locations = state.abb, locationmode = 'USA-states') %>%
  layout(geo = g)

vars <- colnames(state.x77)
barcharts <- lapply(vars, function(var) {
  plot_ly(x = state.x77[, var], y = state.name) %>% 
    add_bars(orientation = "h", name = var) %>% 
    layout(showlegend = FALSE, hovermode = "y",
           yaxis = list(showticklabels = FALSE))
}) 

subplot(
  subplot(barcharts, margin = 0.01), map,
  nrows = 2, heights = c(0.3, 0.7), margin = 0.1
)

##
install.packages("gapminder")
library(gapminder)
data(gapminder)

gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()

ggplotly(gg)

###
base <- gapminder %>%
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop,
          text = ~country, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

 suppressMessages(suppressWarnings(base %>%
             add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
                             animation_opts(1000, easing = "elastic", redraw = FALSE) %>% 
               animation_button( x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
                                             animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red"))
                                             )))
 
###
 g <- crosstalk::SharedData$new(gapminder, ~continent)
gg <- ggplot(g, aes(gdpPercap, lifeExp, color = continent, frame = year)) +
  geom_point(aes(size = pop, ids = country)) + geom_smooth(se = FALSE, method = "lm") + scale_x_log10()

 suppressMessages(suppressWarnings(ggplotly(gg) %>%
                                             highlight("plotly_hover")))


