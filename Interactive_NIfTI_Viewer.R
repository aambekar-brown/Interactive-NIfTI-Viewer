library(shiny)
library(shinyFiles)
library(neurobase)
library(oro.nifti)
library(fs)  # For cross-platform file system operations

# Define the user interface
ui <- fluidPage(
  titlePanel(HTML("<b>Interactive NIfTI Viewer</b>"), windowTitle = "NIfTI Viewer"),
  
  
  tags$head(
    tags$style(HTML("
      .console-container {
        height: 250px;
      }
      .visualization-container {
        margin-top: 20px;
      }
      .slice-controls .btn {
        width: 100%;
        margin-bottom: 5px;
      }
    "))
  ),
  
  wellPanel(
    fluidRow(
      column(12,
             h4("NIfTI File Viewer"),
             p("Select one or more NIfTI files to view.")
      )
    ),
    fluidRow(
      column(3, strong("NIfTI File(s) Input Path")),
      column(6, textInput("nifti_input", label = NULL, placeholder = "Path to NIfTI files")),
      column(3, shinyFilesButton("btn_nifti", "Select", title = "Select NIfTI files", multiple = TRUE))
    )
  ),
  
  fluidRow(
    column(12,
           wellPanel(
             div(class = "visualization-container",
                 h4("Output Visualization"),
                 selectInput("selected_output", "Select NIfTI File:", choices = NULL),
                 div(style = "display: flex; align-items: center; gap: 10px;",
                     actionButton("prev_slice", "Previous Slice", class = "btn btn-primary"),
                     sliderInput("slice_number", "Slice Number:", min = 1, max = 1, value = 1, width = "100%"),
                     actionButton("next_slice", "Next Slice", class = "btn btn-primary")
                 ),
                 div(style = "display: flex; align-items: center; gap: 10px;",
                     actionButton("play_slices", "Play", class = "btn btn-success"),
                     actionButton("stop_slices", "Pause", class = "btn btn-warning")
                 ),
                 plotOutput("nifti_plot", width = "100%", height = "600px")
             )
           )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to control the play state
  play_state <- reactiveVal(FALSE)
  
  # Reactive values to store the images and file paths
  nifti_imgs <- reactiveVal(list())
  nifti_paths <- reactiveVal(list())
  
  # Define the volumes for shinyFiles
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  # File chooser logic for NIfTI File Input Path
  shinyFileChoose(input, 'btn_nifti', roots = volumes, session = session)
  
  observeEvent(input$btn_nifti, {
    req(input$btn_nifti)
    files <- shinyFiles::parseFilePaths(volumes, input$btn_nifti)
    if (nrow(files) > 0) {
      selected_paths <- as.character(files$datapath)
      
      # Error handling for long paths
      long_paths <- selected_paths[nchar(selected_paths) > 250]
      if (length(long_paths) > 0) {
        # Show error message to the user
        showModal(modalDialog(
          title = "Path Length Error",
          paste("The following file paths are too long (exceeding 250 characters) and cannot be processed:\n", paste(long_paths, collapse = "\n")),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        # Remove long paths from selected_paths
        selected_paths <- selected_paths[nchar(selected_paths) <= 250]
        # If no valid paths remain, stop processing
        if (length(selected_paths) == 0) {
          return()
        }
      }
      
      updateTextInput(session, "nifti_input", value = paste(selected_paths, collapse = ","))
      
      # Load the NIfTI images
      imgs <- list()
      paths <- list()
      for (path in selected_paths) {
        # Add tryCatch to handle errors during image loading
        img <- tryCatch({
          readNIfTI(path, reorient = FALSE)
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("Failed to load image:", basename(path), "\nError:", e$message),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          NULL  # Return NULL if failed
        })
        if (!is.null(img)) {
          fname <- basename(path)
          imgs[[fname]] <- img
          paths[[fname]] <- path
        }
      }
      
      if (length(imgs) > 0) {
        # Store the images and paths in reactive values
        nifti_imgs(imgs)
        nifti_paths(paths)
        
        # Update the selectInput with file names
        updateSelectInput(session, "selected_output",
                          choices = names(imgs),
                          selected = names(imgs)[1])
      } else {
        showModal(modalDialog(
          title = "No Valid Images",
          "No valid NIfTI images were loaded. Please select valid NIfTI files.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  # Reactive value to store the currently selected image
  current_img <- reactiveVal(NULL)
  
  # Update current image when a new file is selected
  observeEvent(input$selected_output, {
    req(input$selected_output)
    imgs <- nifti_imgs()
    selected_name <- input$selected_output
    if (!is.null(imgs[[selected_name]])) {
      img <- imgs[[selected_name]]
      current_img(img)
      
      # Determine the minimum dimension among x, y, z for slider max
      img_dims <- dim(img)
      min_dim <- min(img_dims[1:3])
      
      updateSliderInput(session, "slice_number", min = 1, max = min_dim, value = round(min_dim / 2))
    }
  })
  
  # Observe Previous Slice button click
  observeEvent(input$prev_slice, {
    play_state(FALSE) # Stop play when manually changing slices
    current_slice <- input$slice_number
    if (current_slice > 1) {
      updateSliderInput(session, "slice_number", value = current_slice - 1)
    }
  })
  
  # Observe Next Slice button click
  observeEvent(input$next_slice, {
    play_state(FALSE) # Stop play when manually changing slices
    current_slice <- input$slice_number
    img <- current_img()
    if (!is.null(img)) {
      img_dims <- dim(img)
      min_dim <- min(img_dims[1:3])
      if (current_slice < min_dim) {
        updateSliderInput(session, "slice_number", value = current_slice + 1)
      }
    }
  })
  
  # Observe Play button click
  observeEvent(input$play_slices, {
    play_state(TRUE)
  })
  
  # Observe Pause button click
  observeEvent(input$stop_slices, {
    play_state(FALSE)
  })
  
  # Automatically update the slice number during play state
  observe({
    req(current_img())
    if (play_state()) {
      current_slice <- input$slice_number
      img_dims <- dim(current_img())
      min_dim <- min(img_dims[1:3])
      if (current_slice < min_dim) {
        updateSliderInput(session, "slice_number", value = current_slice + 1)
        invalidateLater(200, session)  # Adjust the interval as needed (milliseconds)
      } else {
        play_state(FALSE) # Stop play when the last slice is reached
      }
    }
  })
  
  # Render plot for the NIfTI image
  output$nifti_plot <- renderPlot({
    req(current_img())
    slice_num <- as.integer(input$slice_number)
    img <- current_img()
    
    # Extract dimensions
    dims <- dim(img)
    
    # Prepare slices for axial, sagittal, and coronal views
    axial_slice <- if (slice_num <= dims[3]) img[,,slice_num] else matrix(0, nrow = dims[1], ncol = dims[2])
    sagittal_slice <- if (slice_num <= dims[1]) img[slice_num,,] else matrix(0, nrow = dims[2], ncol = dims[3])
    coronal_slice <- if (slice_num <= dims[2]) img[,slice_num,] else matrix(0, nrow = dims[1], ncol = dims[3])
    
    # Set up the plotting area to have 1 row and 3 columns
    par(mfrow = c(1, 3), mar = c(1, 1, 2, 1), oma = c(0, 0, 2, 0))
    
    # Plot axial view
    image(axial_slice, col = gray(0:64/64), axes = FALSE, main = paste("Axial (Z=", slice_num, ")"), asp = 1)
    
    # Plot sagittal view (rotate for proper orientation)
    image(sagittal_slice, col = gray(0:64/64), axes = FALSE, main = paste("Sagittal (X=", slice_num, ")"), asp = 1)
    
    # Plot coronal view (rotate for proper orientation)
    image(coronal_slice, col = gray(0:64/64), axes = FALSE, main = paste("Coronal (Y=", slice_num, ")"), asp = 1)
    
    # Add overall title
    mtext(paste("Slice Number:", slice_num), outer = TRUE, cex = 1.5)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
