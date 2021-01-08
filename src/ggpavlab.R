ggpavplot <- function(data, mapping,
                      type = NULL,
                      title = NULL, subtitle = NULL, caption = NULL, label = NULL) {
  typeOptions <- c('bar', 'point', 'line', 'histogram', 'violin', 'jitter', 'density', 'pie', 'venn', 'heatmap')
  
  if('x' %in% names(mapping))
    x_num <- is.numeric(pull(data, rlang::get_expr(mapping$x)))
  if('y' %in% names(mapping))
    y_num <- is.numeric(pull(data, rlang::get_expr(mapping$y)))
  
  if(is.null(type)) {
    if('x' %in% names(mapping)) {
      if('y' %in% names(mapping)) {
        if(y_num) {
          if(x_num)
            type <- 'point'
          else
            type <- 'jitter'
        }
      } else
        type <- 'histogram'
    }
    
    message(paste0('[PavPlot] Plot type not specified. Made best guess: ',
                   paste0(type, collapse = ', '),
                   '. Specify parameter "type" to override.'))
  }
  
  if(!all(type %in% typeOptions))
    stop(paste('type must be one of', paste0(typeOptions, collapse = ', ')))
  
  mPlot <- ggplot(data, mapping) + theme_pavlab() +
    labs(title = title, subtitle = subtitle, caption = caption, tag = label)
  
  for(mType in type) {
    if(mType %in% c('bar', 'point', 'line', 'histogram', 'violin', 'jitter', 'density')) {
      mPlot <- mPlot + getFunction(paste0('geom_', mType))()
    } else if(mType == 'pie') {
      
    } else if(mType == 'venn') {
      
    } else if(mType == 'heatmap') {
      
    }
  }
  
  for(name in names(mapping)) {
    if(name == 'colour') {
      mPlot <- mPlot + scale_colour_pavlab()
    } else if(name == 'fill') {
      mPlot <- mPlot + scale_fill_pavlab()
    } else if(name == 'shape') {
      mPlot <- mPlot + scale_shape_pavlab()
    } else if(name == 'x') {
      mPlot <- mPlot + scale_x_pavlab(type = if_else(x_num, 'continuous', 'discrete'))
    } else if(name == 'y') {
      mPlot <- mPlot + scale_y_pavlab(type = if_else(y_num, 'continuous', 'discrete'))
    } else if(startsWith(name, 'facet')) {
      mDims <- suppressWarnings(as.integer(stringr::str_match(name, '([0-9]*)x([0-9]*)')[c(2, 3)]))
      name <- gsub('([0-9]*)x([0-9]*)', '', name)
      
      mScales <- case_when(
        grepl('xy', name, fixed = T) ~ 'free',
        grepl('x', name, fixed = T) ~ 'free_x',
        grepl('y', name, fixed = T) ~ 'free_y',
        T ~ 'fixed'
      )
      
      mPlot <- mPlot + facet_wrap(nrow = mDims[1], ncol = mDims[2], scales = mScales)
    } else if(name == 'coord') {
      mPlot <- mPlot + getFunction(paste0('coord_', mapping[[name]]))()
    } else if(name == 'flip') {
      if(isTRUE(eval(rlang::get_expr(mapping[[name]]))))
        mPlot <- mPlot + coord_flip()
    }
  }
  
  mPlot
}

theme_pavlab <- function(font_size = 21,
                         font_family = '',
                         line_size = 1/2,
                         rel_small = 12/14,
                         rel_tiny = 11/14,
                         rel_large = 16/14,
                         axis_x_angle = 0,
                         legend.position = 'right')  {
  require(cowplot)
  theme_cowplot(font_size, font_family, line_size, rel_small, rel_tiny, rel_large) %+replace%
    theme(strip.background = element_blank(),
          axis.text.x.bottom = element_text(angle = axis_x_angle,
                                            hjust = case_when(
                                              axis_x_angle == 45 ~ 1,
                                              axis_x_angle == 0 ~ 0.5,
                                              axis_x_angle %% 90 == 0 ~ 1,
                                              T ~ 0.5),
                                            vjust = case_when(
                                              axis_x_angle == 90 ~ 0.5,
                                              T ~ 0
                                            )),
          legend.position = legend.position)
}

scale_x_pavlab <- function(type = 'continuous',
                           name = waiver(),
                           limits = NULL,
                           breaks = waiver(),
                           as_percentages = F,
                           no_expand = T) {
  typeOptions <- c('continuous', 'discrete', 'binned')
  
  if(!(type %in% typeOptions))
    stop(paste('type must be one of', paste0(typeOptions, collapse = ', ')))
  
  labs <- switch(as_percentages + 1, waiver(), scales::percent)
  exp <- switch(no_expand + 1, NULL, rep(0, 2))
  
  if(type == 'continuous')
    scale_x_continuous(name = name, limits = limits, breaks = breaks, expand = exp, labels = labs)
  else if(type == 'discrete')
    scale_x_discrete(name = name, limits = limits, breaks = breaks, expand = exp, labels = labs)
  else if(type == 'binned')
    scale_x_binned(name = name, limits = limits, breaks = breaks, expand = exp, labels = labs)
}

scale_y_pavlab <- function(type = 'continuous',
                           name = waiver(),
                           limits = NULL,
                           as_percentages = F,
                           no_expand = T) {
  typeOptions <- c('continuous', 'discrete', 'binned')
  
  if(!(type %in% typeOptions))
    stop(paste('type must be one of', paste0(typeOptions, collapse = ', ')))
  
  labs <- switch(as_percentages + 1, waiver(), scales::percent)
  exp <- switch(no_expand + 1, NULL, rep(0, 2))
  
  if(type == 'continuous')
    scale_y_continuous(name = name, limits = limits, expand = exp, labels = labs)
  else if(type == 'discrete')
    scale_y_discrete(name = name, limits = limits, expand = exp, labels = labs)
  else if(type == 'binned')
    scale_y_binned(name = name, limits = limits, expand = exp, labels = labs)
}

scale_color_pavlab <- function(type = 'continuous',
                               name = waiver(),
                               palette = 'Dark2') {
  typeOptions <- c('continuous', 'discrete', 'binned', 'brewer')
  
  if(!(type %in% typeOptions))
    stop(paste('type must be one of', paste0(typeOptions, collapse = ', ')))
  
  if(type == 'continuous')
    scale_color_continuous(name = name)
  else if(type == 'discrete')
    scale_y_discrete(name = name, limits = limits, expand = exp, labels = labs)
  else if(type == 'binned')
    scale_y_binned(name = name, limits = limits, expand = exp, labels = labs)
  else if(type == 'brewer')
    scale_color_brewer(name = name, palette = palette)
}

scale_fill_pavlab <- function(type = 'continuous',
                              name = waiver(),
                              palette = 'Dark2') {
  
}

scale_shape_pavlab <- function(type = 'continuous',
                               name = waiver()) {
  
}
