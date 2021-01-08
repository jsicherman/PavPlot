ggpavplot(iris,
          aes(Species, Sepal.Length),
          c('violin', 'jitter'),
          title = 'Test',
          subtitle = 'PavPlot',
          caption = 'An example PavPlot',
          label = 'A')

ggpavplot(iris,
          aes(Sepal.Width, Sepal.Length))
