#!/usr/bin/env Rscript

COLUMN_MAX <- config$COLUMN_WIDTH

# -------------------------------------------------------------------
# 1) Load Required Packages
# -------------------------------------------------------------------
if (!requireNamespace("tcltk", quietly = TRUE)) {
  stop("Package 'tcltk' is required for this GUI. Please ensure it is installed.")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is needed for XLS/XLSX reading. Please install it.")
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is needed for XLSX writing. Please install it.")
}

library(tcltk)
library(readxl)
library(openxlsx)

launch_app <- function() {

  # -------------------------------------------------------------------
  # 3) Global Variables / State
  # -------------------------------------------------------------------
  my_data1 <- NULL       # Data for Table 1
  my_data2 <- NULL       # Data for Table 2
  file_path1 <- ""       # File path for Table 1
  file_path2 <- ""       # File path for Table 2

  my_win <- NULL         # Main Tk window

  # Widgets for Table 1
  listbox_cols1 <- NULL
  preview_text1 <- NULL
  selected_columns_label1 <- NULL
  file_path_label1 <- NULL

  # Widgets for Table 2
  listbox_cols2 <- NULL
  preview_text2 <- NULL
  selected_columns_label2 <- NULL
  file_path_label2 <- NULL

  # -------------------------------------------------------------------
  # 4) Helper Functions
  # -------------------------------------------------------------------

  # Truncate large strings for preview
  limit_text <- function(x, max_len = 30) {
    x_chr <- as.character(x)
    too_long <- nchar(x_chr, type = "width") > max_len
    x_chr[too_long] <- paste0(substr(x_chr[too_long], 1, max_len), "...")
    x_chr
  }

  # Format table with padded columns for proper alignment
  format_table_for_preview <- function(df) {
    if (nrow(df) == 0) return("")

    df[] <- lapply(df, function(col) {
      if (is.character(col) || is.factor(col)) {
        limit_text(col, COLUMN_MAX)
      } else {
        col
      }
    })

    # Use display width to compute column widths
    col_widths <- sapply(names(df), function(col_name) {
      max(nchar(c(as.character(df[[col_name]]), col_name), type = "width"))
    })
    col_widths <- col_widths + 3

    # Function to pad string to desired display width
    pad_to_width <- function(x, width) {
      x <- as.character(x)
      actual_width <- nchar(x, type = "width")
      padding <- pmax(0, width - actual_width)
      paste0(x, strrep(" ", padding))
    }

    format_row <- function(row) {
      paste(mapply(pad_to_width, limit_text(row), col_widths), collapse = "")
    }

    header <- format_row(names(df))
    data_rows <- apply(df, 1, format_row)

    paste(c(header, data_rows), collapse = "\n")
  }

  refresh_preview <- function(df, preview_text) {
    if (is.null(df)) return()
    df_disp <- head(df, 10)
    formatted_table <- format_table_for_preview(df_disp)
    tkdelete(preview_text, "1.0", "end")
    tkinsert(preview_text, "end", formatted_table)
  }

  update_selected_columns <- function(df, listbox_cols, selected_columns_label) {
    sel_indices <- as.integer(tkcurselection(listbox_cols))
    if (length(sel_indices) == 0) {
      tkconfigure(selected_columns_label, text = "Selected columns: None")
    } else {
      selected_colnames <- names(df)[sel_indices + 1]
      tkconfigure(selected_columns_label, text = paste("Selected columns:", paste(selected_colnames, collapse = ", ")))
    }
  }

  load_file <- function(assign_var, listbox_cols, preview_text, selected_columns_label, file_path_label, file_path_var) {
    fpath <- tk_choose.files(
      caption = "Select a CSV or XLS(X) file",
      filter = matrix(c(
        "CSV Files", "*.csv",
        "Excel Files", "*.xlsx;*.xls",
        "All Files", "*"
      ), ncol = 2, byrow = TRUE)
    )
    if (length(fpath) == 0 || fpath == "") return()

    ext <- tolower(tools::file_ext(fpath))
    df <- NULL
    if (ext == "csv") {
      df <- read.csv(fpath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- read_excel(fpath)
      df <- as.data.frame(df, stringsAsFactors = FALSE)
    } else {
      tkmessageBox(message = "Unsupported file type.", icon = "error")
      return()
    }

    assign(assign_var, df, envir = .GlobalEnv)
    assign(file_path_var, fpath, envir = .GlobalEnv)
    tkconfigure(file_path_label, text = paste("Loaded File:", fpath))

    tkdelete(listbox_cols, 0, "end")
    for (colname in names(df)) {
      tkinsert(listbox_cols, "end", colname)
    }

    refresh_preview(df, preview_text)
    update_selected_columns(df, listbox_cols, selected_columns_label)
  }

  hash_columns <- function(assign_var, listbox_cols, preview_text) {
    df <- get(assign_var, envir = .GlobalEnv)
    if (is.null(df)) {
      tkmessageBox(message = "No data loaded.", icon = "warning")
      return()
    }

    sel_indices <- as.integer(tkcurselection(listbox_cols))
    if (length(sel_indices) < 1) {
      tkmessageBox(message = "Please select at least one column.", icon = "warning")
      return()
    }

    selected_colnames <- names(df)[sel_indices + 1]
    df$HASH <- sapply(seq_len(nrow(df)), function(i) {
      combined_text <- paste0(df[i, selected_colnames], collapse = "#")
      generate_hashed_substrings(combined_text)
    })

    df <- df[, !(names(df) %in% selected_colnames)]
    assign(assign_var, df, envir = .GlobalEnv)

    tkdelete(listbox_cols, 0, "end")
    for (colname in names(df)) {
      tkinsert(listbox_cols, "end", colname)
    }

    refresh_preview(df, preview_text)
  }

  save_file <- function(df) {
    if (is.null(df)) {
      tkmessageBox(message = "No data to save.", icon = "warning")
      return()
    }

    fpath <- tclvalue(tkgetSaveFile(
      filetypes = '{"CSV Files" ".csv"} {"Excel Files" ".xlsx"} {"All Files" "*"}',
      defaultextension = ".csv"
    ))

    if (!nzchar(fpath)) return()

    ext <- tolower(tools::file_ext(fpath))

    if (ext == "csv") {
      write.csv(df, fpath, row.names = FALSE)
      tkmessageBox(message = paste("Data saved to CSV:", fpath))
    } else if (ext == "xlsx") {
      openxlsx::write.xlsx(df, fpath)
      tkmessageBox(message = paste("Data saved to XLSX:", fpath))
    } else {
      write.csv(df, fpath, row.names = FALSE)
      tkmessageBox(message = paste("Data saved as CSV (unrecognized extension). File:", fpath))
    }
  }

  # -------------------------------------------------------------------
  # 5) Build the Tcl/Tk GUI
  # -------------------------------------------------------------------
  {
    my_win <- tktoplevel()
    tkwm.withdraw(my_win)
    tkwm.title(my_win, "Hashing Columns App (tcltk)")

    # Create a main frame to organize two vertically split sections
    main_frame <- tkframe(my_win)
    frame_left <- tkframe(main_frame)
    frame_right <- tkframe(main_frame)

    tkpack(main_frame, fill = "both", expand = TRUE)
    tkpack(frame_left, side = "left", fill = "both", expand = TRUE, padx = 5, pady = 5)
    tkpack(frame_right, side = "right", fill = "both", expand = TRUE, padx = 5, pady = 5)

    # Frames for Table 1 (left section)
    frame_buttons1 <- tkframe(frame_left)
    frame_listbox1 <- tkframe(frame_left)
    frame_preview1 <- tkframe(frame_left)

    # Frames for Table 2 (right section)
    frame_buttons2 <- tkframe(frame_right)
    frame_listbox2 <- tkframe(frame_right)
    frame_preview2 <- tkframe(frame_right)

    # Pack frames for both tables
    tkpack(frame_buttons1, side = "top", fill = "x", padx = 10, pady = 5)
    tkpack(frame_listbox1, side = "top", fill = "y", padx = 10, pady = 5)
    tkpack(frame_preview1, side = "top", fill = "both", expand = TRUE, padx = 10, pady = 5)

    tkpack(frame_buttons2, side = "top", fill = "x", padx = 10, pady = 5)
    tkpack(frame_listbox2, side = "top", fill = "y", padx = 10, pady = 5)
    tkpack(frame_preview2, side = "top", fill = "both", expand = TRUE, padx = 10, pady = 5)

    # Buttons for Table 1
    btn_load1 <- tkbutton(frame_buttons1, text = "Load File (Table 1)", command = function() load_file("my_data1", listbox_cols1, preview_text1, selected_columns_label1, file_path_label1, "file_path1"))
    btn_hash1 <- tkbutton(frame_buttons1, text = "Hash Columns (Table 1)", command = function() hash_columns("my_data1", listbox_cols1, preview_text1))
    btn_save1 <- tkbutton(frame_buttons1, text = "Save File (Table 1)", command = function() save_file(my_data1))
    tkpack(btn_load1, btn_hash1, btn_save1, side = "left", padx = 5)

    # Buttons for Table 2
    btn_load2 <- tkbutton(frame_buttons2, text = "Load File (Table 2)", command = function() load_file("my_data2", listbox_cols2, preview_text2, selected_columns_label2, file_path_label2, "file_path2"))
    btn_hash2 <- tkbutton(frame_buttons2, text = "Hash Columns (Table 2)", command = function() hash_columns("my_data2", listbox_cols2, preview_text2))
    btn_save2 <- tkbutton(frame_buttons2, text = "Save File (Table 2)", command = function() save_file(my_data2))
    tkpack(btn_load2, btn_hash2, btn_save2, side = "left", padx = 5)

    # Listbox for column selection (Table 1)
    lbl_cols1 <- tklabel(frame_listbox1, text = "Select columns (Table 1):")
    scrollbar_listbox1 <- tkscrollbar(frame_listbox1, orient = "vertical", command = function(...) tkyview(listbox_cols1, ...))
    listbox_cols1 <- tklistbox(frame_listbox1, selectmode = "multiple", width = 20, height = 15, yscrollcommand = function(...) tkset(scrollbar_listbox1, ...))
    tkpack(lbl_cols1, side = "top")
    tkpack(listbox_cols1, side = "left", fill = "y")
    tkpack(scrollbar_listbox1, side = "right", fill = "y")

    # File path label for Table 1
    file_path_label1 <- tklabel(frame_listbox1, text = "Loaded File: None")
    tkpack(file_path_label1, side = "top", pady = 5)

    # Listbox for column selection (Table 2)
    lbl_cols2 <- tklabel(frame_listbox2, text = "Select columns (Table 2):")
    scrollbar_listbox2 <- tkscrollbar(frame_listbox2, orient = "vertical", command = function(...) tkyview(listbox_cols2, ...))
    listbox_cols2 <- tklistbox(frame_listbox2, selectmode = "multiple", width = 20, height = 15, yscrollcommand = function(...) tkset(scrollbar_listbox2, ...))
    tkpack(lbl_cols2, side = "top")
    tkpack(listbox_cols2, side = "left", fill = "y")
    tkpack(scrollbar_listbox2, side = "right", fill = "y")

    # File path label for Table 2
    file_path_label2 <- tklabel(frame_listbox2, text = "Loaded File: None")
    tkpack(file_path_label2, side = "top", pady = 5)

    # Text preview area for Table 1
    lbl_preview1 <- tklabel(frame_preview1, text = "Data Preview (Table 1, first 10 rows):")
    scrollbar_preview_y1 <- tkscrollbar(frame_preview1, orient = "vertical", command = function(...) tkyview(preview_text1, ...))
    scrollbar_preview_x1 <- tkscrollbar(frame_preview1, orient = "horizontal", command = function(...) tkxview(preview_text1, ...))
    preview_text1 <- tktext(frame_preview1, wrap = "none", width = 80, height = 20, yscrollcommand = function(...) tkset(scrollbar_preview_y1, ...), xscrollcommand = function(...) tkset(scrollbar_preview_x1, ...))
    tkpack(lbl_preview1, side = "top", anchor = "w")
    tkpack(preview_text1, side = "top", fill = "both", expand = TRUE)
    tkpack(scrollbar_preview_y1, side = "right", fill = "y")
    tkpack(scrollbar_preview_x1, side = "bottom", fill = "x")

    # Text preview area for Table 2
    lbl_preview2 <- tklabel(frame_preview2, text = "Data Preview (Table 2, first 10 rows):")
    scrollbar_preview_y2 <- tkscrollbar(frame_preview2, orient = "vertical", command = function(...) tkyview(preview_text2, ...))
    scrollbar_preview_x2 <- tkscrollbar(frame_preview2, orient = "horizontal", command = function(...) tkxview(preview_text2, ...))
    preview_text2 <- tktext(frame_preview2, wrap = "none", width = 80, height = 20, yscrollcommand = function(...) tkset(scrollbar_preview_y2, ...), xscrollcommand = function(...) tkset(scrollbar_preview_x2, ...))
    tkpack(lbl_preview2, side = "top", anchor = "w")
    tkpack(preview_text2, side = "top", fill = "both", expand = TRUE)
    tkpack(scrollbar_preview_y2, side = "right", fill = "y")
    tkpack(scrollbar_preview_x2, side = "bottom", fill = "x")

    # Labels for selected columns
    selected_columns_label1 <- tklabel(frame_preview1, text = "Selected columns: None", anchor = "w", wraplength = 600)
    tkpack(selected_columns_label1, side = "top", pady = 5)

    selected_columns_label2 <- tklabel(frame_preview2, text = "Selected columns: None", anchor = "w", wraplength = 600)
    tkpack(selected_columns_label2, side = "top", pady = 5)

    # Deiconify and start event loop
    tkwm.deiconify(my_win)
    tkfocus(my_win)
    tkwait.window(my_win)
  }

}
