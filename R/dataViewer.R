#' an interactive addin to help visualise data and identifiy outliers
#'
#' an interactive addin to help visualise data and identify outliers
#'
#' @param data a data frame that contains the raw data
#' @param x the x variable
#' @param y the y variable
#' @param id_col the column that splits the data frame into separate plots
#' @param col an optional variable if different colours are desired on each plot
#' @param predictions a data frame that contains the predictions if desired
#' @param group an optional variable for grouping model predictions when there are multiple predictions on a single plot
#' @param lm_fit whether or not you want a linear model fit to be superimposed over the data. Defaults to FALSE
#' @return a dataframe of points that were clicked
#' @description opens a pane from which you can select each set of data and select points to be dropped. The undo button gets rid of the last selection. Press "DONE" to get a dataframe of the selected points.
#' @export

dataViewer <- function(data, x, y, predictions = NULL, id_col = NULL, col = NULL, group = NULL, lm_fit = FALSE){

    # delete NAs from dataset
    data <- data[!is.na(data[,y]),]
    data <- data[!is.na(data[,x]),]

    # colnames
    cols <- colnames(data)

    # one id column
    if(is.null(id_col)){
      id_col <-  'id_col'
      data$id_col <- 'ALL THE DATA'}

    # create all id_col
    id <- unique(as.character(data[,id_col]))

    # if missing col
    if(is.null(col)){
      data$col <- 'black'}
    if(is.null(group)){
      group = id_col}

    # define the UI for the gadget
    ui <- miniUI::miniPage(
      # set title
      miniUI::gadgetTitleBar("dataViewer"),
      # set up content panel
      miniUI::miniContentPanel(
                  shiny::selectInput("data", "Choose curve:", choices = id),
                  shiny::plotOutput("plot1",
                           click = "plot1_click",
                           brush = shiny::brushOpts(id = 'plot1_brush',
                                                    resetOnNew = TRUE))),
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
        dat <- data[data[,id_col] == input$data,]
        keep    <- get_unclicked(dat, vals$deleted_rows)
        exclude <- get_clicked(dat, vals$deleted_rows)

        ggplot2::update_geom_defaults("smooth", list(colour = 'red', fill = 'red'))
        ggplot2::update_geom_defaults("line", list(colour = 'red', linetype = 2))

        # no predictions
        if(is.null(predictions)){
          if(lm_fit == TRUE){



            # plot 1
            ggplot2::ggplot() +
              ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), size = 3, keep) +
              ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), shape = 21, size = 3, exclude, alpha = 0.75) +
              ggplot2::theme_bw(base_size = 18, base_family = 'Helvetica') +
              ggplot2::ggtitle(input$data) +
              ggplot2::stat_smooth(ggplot2::aes_string(x = x, y = y, col = col, fill = col), method = 'lm', se = T, keep)
          } else{
            ggplot2::ggplot() +
              ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), size = 3, keep) +
              ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), shape = 21, size = 3, exclude, alpha = 0.75) +
              ggplot2::theme_bw(base_size = 18, base_family = 'Helvetica') +
              ggplot2::ggtitle(input$data)
            }
          } else {
          # with predictions
          preds <- predictions[predictions[,id_col] == input$data,]
          # plot 1
          ggplot2::ggplot() +
            ggplot2::geom_line(ggplot2::aes_string(x = x, y = y, col = col, group = group), linetype = 2, size = 1.5, preds) +
            ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), size = 3, keep) +
            ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col), shape = 21, size = 3, exclude, alpha = 0.75) +
            ggplot2::theme_bw(base_size = 18, base_family = 'Helvetica') +
            ggplot2::ggtitle(input$data)
            }

    })

      # Make points that are clicked turn grey
      shiny::observeEvent(input$plot1_click,{
        dat <- data[data[,id_col] == input$data,]
        vals$number_of_points <- c(vals$number_of_points, 1)
        vals$deleted_rows <- rbind(vals$deleted_rows, shiny::nearPoints(dat, input$plot1_click, allRows = FALSE))
      })

      shiny::observeEvent(input$plot1_brush,{
        dat <- data[data[,id_col] == input$data,]
        vals$number_of_points <- c(vals$number_of_points, nrow(shiny::brushedPoints(dat, input$plot1_brush, allRows = FALSE)))
        vals$deleted_rows <- rbind(vals$deleted_rows, shiny::brushedPoints(dat, input$plot1_brush, allRows = FALSE))
      })

      # Undo last click
      shiny::observeEvent(input$undo_last_point,{
        last_row <- tail(vals$number_of_points, 1)
        vals$number_of_points <- vals$number_of_points[-length(vals$number_of_points)]

        vals$deleted_rows <- head(vals$deleted_rows, nrow(vals$deleted_rows) - last_row)

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
    shiny::runGadget(ui, server, viewer = shiny::dialogViewer('dataViewer', width = 700, height = 1200))
  }

