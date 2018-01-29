#' an interactive addin to help visualise data and identifiy outliers
#'
#' an interactive addin to help visualise data and identify outliers
#'
#' @param data a data frame that contains the raw data
#' @param x the x variable
#' @param y the y variable
#' @param id_cols the columns that split the data frame into separate plots
#' @param col an optional variable if different colours are desired on each plot
#' @param predictions a data frame that contains the predictions if desired
#' @param group an optional variable for grouping model predictions when there are multiple predictions on a single plot
#' @param lm_fit whether or not you want a linear model fit to be superimposed over the data. Defaults to FALSE
#' @return a dataframe of points that were clicked
#' @description opens a pane from which you can select each set of data and select points to be dropped. The undo button gets rid of the last selection. Press "DONE" to get a dataframe of the selected points.
#' @export

dataViewer <- function(data, x, y, predictions = NULL, id_cols = NULL, col = NULL, group = NULL, lm_fit = FALSE){

  # delete NAs from dataset
  data <- data[!is.na(data[,y]),]
  data <- data[!is.na(data[,x]),]

  # colnames
  cols <- colnames(data)

  # one id column
  if(is.null(id_cols)){
    data[,'id_col'] <- 'ALL THE DATA'
    if(!is.null(predictions)){
      predictions[,'id_col' <- 'ALL THE DATA']
    }}
  if(length(id_cols) == 1){
    data[,'id_col'] <- data[,id_cols]
    if(!is.null(predictions)){
      predictions[,'id_col'] <- predictions[,id_cols]
    }}
  if(length(id_cols) > 1){
    data <- tidyr::unite_(data, 'id_col', c(id_cols), sep = ' ', remove = F)
    if(!is.null(predictions)){
      predictions <- tidyr::unite_(predictions, 'id_col', c(id_cols), sep = ' ', remove = F)
    }
  }

  # create column for col if it missing to go to defaults
  if(!is.null(col)){data[,'col'] <- 'You cannot handle the colour'}

  # create all id_col
  data <- data.frame(data)
  id <- unique(as.character(data[,'id_col']))

  # define the UI for the gadget
  ui <- miniUI::miniPage(
    # set title
    miniUI::gadgetTitleBar("dataViewer"),
    # set up content panel
    miniUI::miniContentPanel(
      shiny::fillCol(shiny::selectInput("data", "Choose curve:", choices = id),
                     shiny::plotOutput("plot1",
                                       click = "plot1_click",
                                       brush = shiny::brushOpts(id = 'plot1_brush',
                                                                resetOnNew = TRUE), height = '100%'), flex = c(NA,1)), padding = 10),
    miniUI::miniButtonBlock(
      shiny::actionButton("go_to_previous", "Previous"),
      shiny::actionButton("undo_last_point", "Undo"),
      shiny::actionButton("go_to_next", "Next")
    )
  )

  server <- function(input, output, session){
    # For storing which rows have been excluded
    vals <- shiny::reactiveValues(
      deleted_rows = data[FALSE,],
      number_of_points = NULL
    )

    # Define plot1
    output$plot1 <- shiny::renderPlot({
      # subset for keep rows
      dat <- data[data[,'id_col'] == input$data,]
      keep    <- dataViewer::get_unclicked(dat, vals$deleted_rows)
      exclude <- dataViewer::get_clicked(dat, vals$deleted_rows)

      if(!is.null(predictions)){preds <- predictions[predictions[,'id_col'] == input$data,]}

      # no predictions
      plot1 <- ggplot2::ggplot() +
        ggplot2::ggtitle(input$data) +
        ggplot2::theme_bw(base_size = 18, base_family = 'Helvetica')
      if(!is.null(col)){
        plot1 <- plot1 +  ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), size = 3, keep) +
          ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), shape = 21, size = 3, exclude, alpha = 0.75)}
      if(!is.null(col) & !is.null(predictions)){
        plot1 <- plot1 + ggplot2::geom_line(ggplot2::aes_string(x = x, y = y, col = col, group = group), linetype = 2, size = 1.5, preds)}
      if(!is.null(col) & lm_fit == TRUE){
        plot1 <- plot1 + ggplot2::stat_smooth(ggplot2::aes_string(x = x, y = y, col = col, fill = col), method = 'lm', se = T, keep)}
      if(is.null(col)){
        plot1 <- plot1 +  ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), size = 3, keep) +
          ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), shape = 21, size = 3, exclude, alpha = 0.75)}
      if(is.null(col) & !is.null(predictions)){
        plot1 <- plot1 + ggplot2::geom_line(ggplot2::aes_string(x = x, y = y, group = group), col = 'red', linetype = 2, size = 1.5, preds)}
      if(is.null(col) & lm_fit == TRUE){
        plot1 <- plot1 + ggplot2::stat_smooth(ggplot2::aes_string(x = x, y = y), col = 'red', fill = 'red', method = 'lm', se = T, keep)}

      plot1
    })

    # Make points that are clicked turn grey
    shiny::observeEvent(input$plot1_click,{
      dat <- data[data[,'id_col'] == input$data,]
      vals$number_of_points <- c(vals$number_of_points, 1)
      vals$deleted_rows <- rbind(vals$deleted_rows, shiny::nearPoints(dat, input$plot1_click, allRows = FALSE))
    })

    shiny::observeEvent(input$plot1_brush,{
      dat <- data[data[,'id_col'] == input$data,]
      vals$number_of_points <- c(vals$number_of_points, nrow(shiny::brushedPoints(dat, input$plot1_brush, allRows = FALSE)))
      vals$deleted_rows <- rbind(vals$deleted_rows, shiny::brushedPoints(dat, input$plot1_brush, allRows = FALSE))
    })

    # Undo last click
    shiny::observeEvent(input$undo_last_point,{
      last_row <- utils::tail(vals$number_of_points, 1)
      vals$number_of_points <- vals$number_of_points[-length(vals$number_of_points)]

      vals$deleted_rows <- utils::head(vals$deleted_rows, nrow(vals$deleted_rows) - last_row)

    })

    # Move to next id
    shiny::observeEvent(input$go_to_next, {
      current_id <- input$data
      next_id <- id[match(current_id, id) + 1]
      shiny::updateSelectInput(session, 'data', choices = id, selected = next_id)
    })

    # Move to previous id
    shiny::observeEvent(input$go_to_previous, {
      current_id <- input$data
      next_id <- id[match(current_id, id) - 1]
      shiny::updateSelectInput(session, 'data', choices = id, selected = next_id)
    })

    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {

      deleted_rows <- vals$deleted_rows
      deleted_rows <- deleted_rows[,colnames(deleted_rows) %in% cols]
      # Return the kept points ###
      shiny::stopApp(
        deleted_rows
      )
    })

  }
  # Run the app in the dialog viewer
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer('dataViewer', width = 900, height = 700))
}

