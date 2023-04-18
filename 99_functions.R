# 99_functions.R
# functions for RIA census (copied from career disruption)
# March 2023

## make separate legend for plotting
# from https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
g_legend = function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# to replace missing with zero
replace_zero = function(x){replace_na(x, '0')}

# replace character missing
replace_miss = function(x){
  x[x==''] = NA
  return(x)
}

# date formating
my.date.format = function(x){y= format(x, "%d %b %Y"); return(y)} 

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
# Capitalize the first letter in a word string in R
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


## Make a nice table (single variable version) ##
make_table = function(indata,  # survey data
                      inlabels,  # survey labels
                      include.missing = TRUE, # include missing or not
                      ordered = FALSE, # use defined ordering for categories
                      label, # label to use
                      bar_type = 'percent', # plot numbers or percent
                      add_ci = FALSE, # give 95% confidence interval for random sample
                      ltype = 'label', # is the above label for the 'question' or the 'label'
                      digits = 0, # digits for continuous
                      wrap_labels = NULL, # add carriage returns to labels in plot
                      stack = TRUE, # stack style plot or side by side?
                      rel_widths = c(1.4, 1), # relative width for pyramid plots
                      sample_colours = sample_colours, # colours for random and non-random sample
                      type = 'categorical',
                      plot.type = 'histogram'){
  
  # null plots needed for saving
  plot = NULL
  
  # select the variable and rename
  if(ltype == 'label'){
    var = filter(inlabels, labels == label) %>% pull(names)
  }
  if(ltype == 'question'){
    var = label
  }
  n = names(indata)
  names(indata)[n == var] = 'var'
  
  # remove missing or ...
  if(include.missing == FALSE){
    indata = filter(indata, !is.na(var))
  }
  # ... make missing into a category
  if(include.missing == TRUE){
    indata = mutate(indata,
                    var = ifelse(is.na(var), 'Missing', var))
  }
  
  # ordering
  if(ordered == TRUE){
    olabels = filter(ordering, question == label)
    any.missing = any(indata$var == 'Missing')
    if(include.missing == FALSE | any.missing == FALSE){
      olabels = filter(olabels, var != 'Missing')
    }
    indata = mutate(indata, 
                    var = factor(var, levels = olabels$var))
  }
  
  ### categorical ###
  if(type == 'categorical'){
    
    ### a) table
    counts = group_by(indata, var) %>%
      tally() 
    tab = mutate(counts,
        var = as.character(var),
        var = ifelse(is.na(var)==TRUE, 'Missing', var), # missing response
        percent = prop.table(n),
        percent = roundz(percent*100),
        cell = paste(n, ' (', percent, ')', sep='')) %>%
      dplyr::select(-n, -percent) %>%
      rename('n (%)' = 'cell') %>%
      mutate_if(is.character, replace_zero) # replace missing with zero
    # rename column header
    names(tab)[1] = 'Response' 
    #
    ftab = flextable(tab) %>%
      theme_box() %>%
      autofit()
    
    ### b) back-to-back bar plot
    # create numbers and percents
    for_plot = mutate(counts,
             p = prop.table(n)*100)
    if(bar_type == 'percent'){
      for_plot = mutate(for_plot, y=p)
      ylab = 'Percent'
    }
    if(bar_type == 'number'){
      for_plot = mutate(for_plot, y=n)
      ylab = 'Frequency'
    }
    # wrap labels
    if(is.null(wrap_labels) == FALSE){for_plot = mutate(for_plot, var = str_wrap(var, width = wrap_labels))}
    #
    plot = ggplot(data=for_plot, aes(x=var, y=y, fill=var))+
      g.theme+
      scale_y_reverse()+ # reverse order
      theme(legend.position = 'none')+
      ylab(ylab)+
      xlab('')+
      coord_flip(clip = 'off')+
      theme(plot.margin = margin(0, 2, 0, 0, "mm") ) # t, r, b, l
    # version with numbers
    if(stack == FALSE){
      plot = plot + geom_bar(stat='identity', fill = sample_colours[1])
    }
    if(stack == TRUE){
      plot = plot + 
        geom_bar(stat='identity') + 
        scale_fill_manual(NULL, values = cbPalette)
    }
    #
    if(ordered == TRUE){
      olabels = rev(olabels$var)
      if(is.null(wrap_labels) == FALSE){olabels = str_wrap(olabels, width = wrap_labels)}
      plot = plot + 
        scale_x_discrete(limits = olabels)  # Missing at bottom
    }
    
    # use consistent upper limit for axes
    y.max = max(abs(ggplot_build(plot)$layout$panel_params[[1]]$x.range))
    # 
    if(bar_type == 'number'){
      plot = plot + scale_y_reverse(limits=c(y.max, 0))
    }
    if(bar_type == 'percent'){ # add % to axis labels
      plot = plot + scale_y_reverse(limits=c(y.max, 0), labels = scales::percent_format(scale=1))
    }
    
  }

  ### continuous ###
  if(type == 'continuous'){
    
    ## a) table
    tab = summarise(indata,
                    median = roundz(quantile(var, 0.5, na.rm=TRUE), digits),
                lower = roundz(quantile(var, 0.25, na.rm=TRUE), digits),
                upper = roundz(quantile(var, 0.75, na.rm=TRUE), digits)) 
    # 
    tab = mutate(tab, cell = paste(median, ' (', lower, ' to ', upper, ')', sep='')) %>%
      dplyr::select(cell) %>%
      rename('Median (1st to 3rd quartile)' = 'cell')
    names(tab)[1] = 'Median (1st to 3rd quartile)' # rename column header
    ftab = flextable(tab) %>%
      theme_box() %>%
      autofit()
    
    # plots
    ylab = filter(inlabels, names==label) %>% pull(labels)
    ## b) box plot ...
    if(plot.type == 'boxplot'){
    plot = ggplot(data=indata, aes(x=var))+
      geom_boxplot(col = sample_colours[1])+
      xlab('')+
      ylab(ylab)+
      g.theme+
      coord_flip()
    }
    
    ## b) ... or histogram
    if(plot.type == 'histogram'){
      plot = ggplot(data=indata, aes(x=var))+
      geom_histogram(fill = sample_colours[1], col='grey66')+
      ylab('Count')+
      xlab(ylab)+
      g.theme
    }

  }
  
  ## confidence interval for ...
  ftab_ci = NULL
  if(add_ci == TRUE){
    counts = filter(indata, 
                    var != 'Missing') %>% # exclude missing for this
      group_by(var) %>%
      tally() %>%
      mutate(p = n / sum(n))
    intervals = multinomialCI(counts$n, alpha=0.05)
    cis_raw = bind_cols(counts, as.data.frame(intervals))
    names(cis_raw)[1] = c('Response')
    names(cis_raw)[4:5] = c('lower','upper')
    ftab_ci = mutate(cis_raw,
                     p = roundz(100*p, digits), # turn into rounded percents
                     lower = roundz(100*lower, digits),
                     upper = roundz(100*upper, digits)) %>%
      flextable() %>%
      theme_box() %>%
      autofit()
  }
  
  # return
  return = list()
  return$table = ftab
  return$plot = plot
  return$ci = ftab_ci
  return(return)
      
}


## export levels for table/graph ordering ##
export_levels = function(indata, inlabels, questions){
  for_export = NULL
  n = names(indata)
  for (q in questions){
    #
    temp_data = indata
    names(temp_data)[n == q] = 'var'
    #
    f = dplyr::select(temp_data, var) %>%
      unique() %>%
      mutate(question = q)
    for_export = bind_rows(for_export, f)
  }
  for_export = dplyr::select(for_export, question, var) %>%
    mutate(var = ifelse(is.na(var)==TRUE, 'Missing', var))
  return(for_export)
}


## Make a nice table (multiple variable version) ##
make_table_matrix = function(indata,  # survey data
                      inlabels,  # survey labels
                      legend_order, # for ordering results to match bars
                      bar_colours = c('dark red','grey','dark blue'),
                      include.missing = TRUE, # include missing or not
                      bar_type = 'percent', # plot numbers or percent
                      ordered = FALSE, # use defined ordering for response categories
                      ordered_row = 'Agree', # order rows by frequency of this response
                      remove = '', # text to remove from labels
                      wrap_labels = 40, # use carriage returns to shorten labels in the plot
                      start, # start of the variable name
                      expand_zero = FALSE # use expand on the x-axis to reduce white space
                      ){
  
  # select the variables and put in long format
  long = dplyr::select(indata, 
                       starts_with(start)) %>%
    dplyr::select(!contains('_do_')) %>% # exclude randomisation ordering
    dplyr::select(!contains('_text')) %>% # exclude randomisation ordering
    pivot_longer(cols = everything())

  # remove missing or ...
  if(include.missing == FALSE){
    long = filter(long, !is.na(value))
  }
  # ... make missing into a category
  if(include.missing == TRUE){
    long = mutate(long,
                  value = ifelse(is.na(value), 'Missing', value))
  }

  # get the question labels and remove repeated opening text
  qlabels = filter(inlabels, 
                   str_detect(names, pattern=start),
                   !str_detect(names, pattern='_do_'), # not the random ordering
                   !str_detect(names, pattern='_text') # not the optional text
  ) %>% 
    mutate(labels = str_squish(str_remove(labels, pattern=remove))) %>%
    rename('name' = 'names')
    
  # ordering of responses
  if(ordered == TRUE){
    olabels = filter(ordering, question == start) # need to have ordering data
    any.missing = any(long$value == 'Missing')
    if(include.missing == FALSE | any.missing == FALSE){
      olabels = filter(olabels, var != 'Missing')
    }
    long = mutate(long, 
                  value = factor(value, levels = olabels$var))
  }
  
    ### a) table
    counts = group_by(long, name, value) %>%
      tally() 
    tab = group_by(counts, name) %>%
      mutate(
        percent = prop.table(n),
        percent = roundz(percent*100),
        cell = paste(n, ' (', percent, ')', sep='')) %>%
      dplyr::select(-n, -percent) %>%
      rename('n (%)' = 'cell') %>%
      ungroup() %>%
      mutate_if(is.character, replace_zero) # replace missing with zero
    # order rows by frequency
    if(is.null(ordered_row) == FALSE){
      # version based on frequency
      order = filter(long, value == ordered_row) %>%
        group_by(name) %>%
        tally() %>%
        ungroup() %>%
        arrange(-n) %>% # from high to low
        dplyr::select(-n) %>%
        mutate(rown = 1:n())
      # version based on percent
      order = group_by(long, name, value) %>%
        tally() %>%
        group_by(name) %>%
        mutate(perc = prop.table(n)) %>%
        ungroup() %>%
        filter(value == ordered_row) %>%
        arrange(-perc) %>% # from high to low
        dplyr::select(-value, -perc, -n) %>%
        mutate(rown = 1:n())
      counts = left_join(counts, order, by='name') %>%
        arrange(rown, value)
      tab = left_join(tab, order, by='name') %>%
        arrange(rown, value)
      # add ordering to plot data too
      long = left_join(long, order, by='name')
    }
    tab = left_join(tab, qlabels, by='name') %>% # add long labels
      dplyr::select(labels, everything(), -name) # re-order columns
    # rename column header
    names(tab)[1:2] = c('Question','Response')
    #
    ftab = dplyr::select(tab, -rown) %>%
      arrange(Question, Response) %>% # added Feb 2023
      flextable() %>%
      theme_box() %>%
      merge_v(j=1) %>%
      autofit()
    
    # breaks in labels - wrap labels
    if(is.null(wrap_labels) == FALSE){
      qlabels = mutate(qlabels,
                       labels = str_wrap(labels, width = wrap_labels))
    }
    
    ### b) bar plot
    # create numbers and percents
    for_plot = group_by(counts, name) %>%
      mutate(p = prop.table(n)*100)
    if(bar_type == 'percent'){
      for_plot = mutate(for_plot, y=p)
      ylab = 'Percent'
    }
    if(bar_type == 'number'){
      for_plot = mutate(for_plot, y=n)
      ylab = 'Frequency'
    }
    
#
    if(is.null(ordered_row)==TRUE){
      plot = ggplot(data=for_plot, aes(x=name, y=y, fill=value))+
        scale_x_discrete(labels=qlabels$labels, expand=c(0,0))
    }
    if(is.null(ordered_row)==FALSE){
      olabels = dplyr::select(tab, rown, Question) %>%
        unique()
      if(is.null(wrap_labels)==FALSE){
        olabels = mutate(olabels,
          Question = str_wrap(Question, width=wrap_labels))
      }
      plot = ggplot(data=for_plot, aes(x=rown, y=y, fill=value))+
        scale_x_continuous(breaks = 1:nrow(olabels), labels=olabels$Question, expand=c(0,0))
    }
    
    #
    plot = plot + geom_bar(position='stack', stat='identity')+
      scale_fill_manual(NULL, breaks = legend_order, values = bar_colours)+ # add breaks for ordering
      g.theme+
      scale_y_continuous(expand=c(0,0), labels = scales::percent_format(scale=1))+ # remove gap; use %
      theme(plot.margin = margin(0, 4, -5, 0, "mm"), # t, r, b, l; reduce space at bottom as there's no axis label
            axis.text.x = element_text(size=7), # smaller text due to cramped percents
            panel.spacing = unit(5, "mm"), # increase space between panels to avoid overlap in % labels
            legend.position = 'top',
            legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
            legend.box.margin	= margin(t=0, r=0, b=0, l=0), # reduce space around legend
            legend.margin = margin(t=0, r=0, b=0, l=0, unit='mm'))+ # reduce space around legend
      ylab('')+
      xlab('')+
      coord_flip()

  # return
  return = list()
  return$table = ftab
  return$plot = plot
  return(return)
  
}


## Make a list of comments
# add gender and years of experience to each comment
comments = function(indata,  # survey data
                    question, # label to use
                    plus_label = NULL) # additional label for comments
{
  
  # select the variable and rename
  n = names(indata)
  names(indata)[n == question] = 'var'
  # ditto for plus label
  if(is.null(plus_label) == FALSE){
    names(indata)[n == plus_label] = 'label'
  }
  
  non_missing = filter(indata, 
                       !is.na(var),
                       !tolower(var) %in% c('n/a', 'nil', 'no', 'not applicable') # remove this text
                       ) 
  comments = NULL
  for (k in 1:nrow(non_missing)){
    if(is.null(plus_label)==TRUE){
      comment = paste('* ', non_missing$var[k], '\n', sep='')
    }
    if(is.null(plus_label)==FALSE){
      comment = paste('* ', non_missing$label[k], ') ', non_missing$var[k], '\n', sep='')
    }
    comments = c(comments, comment)
  }
  return(comments)
}


## function to bootstrap uncertainty in key estimates
bootstrap_uncertainty = function(indata, 
                                 n_boot, # number of bootstrap samples
                                 max_possible, # maximum number of answers
                                 n_responded, # number who responded
                                 variable){
  # rename key variable
  index = which(names(indata) == variable)
  names(indata)[index] = 'var'
  # and intervals
  index = which(names(indata) == paste(variable,'_lower',sep=''))
  names(indata)[index] = 'var_lower'
  # and intervals
  index = which(names(indata) == paste(variable,'_upper',sep=''))
  names(indata)[index] = 'var_upper'
  
  not_missing = filter(indata, !is.na(var))
  n_responded = nrow(not_missing)
  
  results = NULL
  for (b in 1:n_boot){
    # numbers for those who responded
    responded = mutate(not_missing,
        var_lower = ifelse(is.na(var_lower), var, var_lower), # if no uncertainty then use mean for lower/upper limit
        var_upper = ifelse(is.na(var_upper), var, var_upper),
        var = runif(n = n(), min = var_lower, max = var_upper)) # account for within-person uncertainty for those who gave an interval
    
    # non-responders
    not_responded = data.frame(var = rep(NA, max_possible - n_responded)) %>%
      mutate(var = sample(responded$var, size = max_possible - n_responded, replace = TRUE))
    
    # 
    frame = bind_rows(responded, not_responded) %>%
      summarise(n = n(),
                zeros = sum(var == 0) / n, # proportion spending no time
                median = median(var),
                mean = mean(var)) %>%
      mutate(boot = b)
    results = bind_rows(results, frame)
  }
  
  return(results)
  
}

## calculate statistics for bootstrap
calc_stats = function(indata, 
                      mult = 10, # multiplier for rounding
                      alpha = 0.05){
  
  # mean
  average = mean(indata$mean)
  lower = quantile(indata$mean, alpha/2)
  upper = quantile(indata$mean, 1 - (alpha/2))
  # round
  average = round(average*mult) / mult
  lower = round(lower*mult) / mult
  upper = round(upper*mult) / mult
  f1 = data.frame(statistic = 'Mean', mean = average, lower = lower, upper = upper)
  
  ## median
  #
  average_median = mean(indata$median)
  lower_median = quantile(indata$median, alpha/2)
  upper_median = quantile(indata$median, 1 - (alpha/2))
  # round
  average_median = round(average_median*mult) / mult
  lower_median = round(lower_median*mult) / mult
  upper_median = round(upper_median*mult) / mult
  f2 = data.frame(statistic = 'Median', mean = average_median, lower = lower_median, upper = upper_median)
  
  ## those spending no time
  #
  none = mean(indata$zeros)
  lower_none = quantile(indata$zeros, alpha/2)
  upper_none = quantile(indata$zeros, 1 - (alpha/2))
  # round, times 100 for percent
  none = round(none*100*mult) / mult
  lower_none = round(lower_none*100*mult) / mult
  upper_none = round(upper_none*100*mult) / mult
  f3 = data.frame(statistic = 'None (%)', mean = none, lower = lower_none, upper = upper_none)

  stats = bind_rows(f1, f2, f3)
  return(stats)
}
