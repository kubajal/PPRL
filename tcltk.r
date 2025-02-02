#!/usr/bin/env Rscript

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
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is needed for hashing. Please install it.")
}

library(tcltk)
library(readxl)
library(openxlsx)
library(digest)

# -------------------------------------------------------------------
# 2) Define Hashing Function
# -------------------------------------------------------------------
generate_hashed_substrings_with_offsets <- function(text, max_position = 1000, offset_range = 2, salt_length = 12) {
  if (nchar(text) < 3) return("")

  precomputed_salts <- sapply(0:(max_position + offset_range), function(i) {
    paste0(sample(c(letters, LETTERS, 0:9), salt_length, replace = TRUE), collapse = "")
  })

  hash_with_salt_and_offset <- function(substring, position) {
    results <- sapply(-offset_range:offset_range, function(offset) {
      new_pos <- position + offset
      if (new_pos >= 0 && new_pos <= max_position) {
        salt <- precomputed_salts[[new_pos + 1]]
        salted_input <- paste0(salt, substring, new_pos)
        hashed_val <- digest(salted_input, algo = "sha512")
        paste0(hashed_val, "_")
      } else {
        NA
      }
    })
    results <- results[!is.na(results)]
    paste(results, collapse = "$")
  }

  substrings <- sapply(seq_len(nchar(text) - 2), function(i) {
    substring_3 <- substr(text, i, i + 2)
    hash_with_salt_and_offset(substring_3, i)
  })

  paste(substrings, collapse = "$")
}

# -------------------------------------------------------------------
# 3) Global Variables / State
# -------------------------------------------------------------------
my_data <- NULL       # Will store the current dataframe
my_win <- NULL        # Main Tk window
listbox_cols <- NULL  # Listbox widget reference
preview_text <- NULL  # Text widget for preview
selected_columns_label <- NULL  # Label to display selected column names

# -------------------------------------------------------------------
# 4) Helper Functions
# -------------------------------------------------------------------

# Truncate large strings for preview
limit_text <- function(x, max_len = 30) {
  x_chr <- as.character(x)
  too_long <- nchar(x_chr) > max_len
  x_chr[too_long] <- paste0(substr(x_chr[too_long], 1, max_len), "...")
  x_chr
}

# Format table with padded columns for proper alignment
format_table_for_preview <- function(df) {
  if (nrow(df) == 0) return("")

  # Truncate large text for preview
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.factor(col)) {
      limit_text(col, 30)
    } else {
      col
    }
  })

  # Calculate maximum column widths (including headers)
  col_widths <- sapply(names(df), function(col_name) {
    max(nchar(c(as.character(df[[col_name]]), col_name)))
  })

  # Add padding between columns (extra spaces)
  col_widths <- col_widths + 3  # Add 3 spaces of padding between columns

  # Function to format a row with padded values
  format_row <- function(row) {
    paste(mapply(sprintf, paste0("%-", col_widths, "s"), row), collapse = "")
  }

  # Format header row and data rows
  header <- format_row(names(df))
  data_rows <- apply(df, 1, format_row)

  paste(c(header, data_rows), collapse = "\n")
}

# Refresh the preview text widget with the first 10 rows
refresh_preview <- function() {
  if (is.null(my_data)) return()

  # Take first 10 rows only
  df_disp <- head(my_data, 10)

  # Format the table for display with padded columns
  formatted_table <- format_table_for_preview(df_disp)

  # Clear the text widget
  tkdelete(preview_text, "1.0", "end")

  # Insert the formatted table into the text widget
  tkinsert(preview_text, "end", formatted_table)
}

# Update the displayed selected column names
update_selected_columns <- function() {
  sel_indices <- as.integer(tkcurselection(listbox_cols))
  if (length(sel_indices) == 0) {
    tkconfigure(selected_columns_label, text = "Selected columns: None")
  } else {
    selected_colnames <- names(my_data)[sel_indices + 1]  # Adjust for 0-based index
    tkconfigure(selected_columns_label, text = paste("Selected columns:", paste(selected_colnames, collapse = ", ")))
  }
}

# Load CSV/XLS(X) file
load_file <- function() {
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

  assign("my_data", df, envir = .GlobalEnv)

  # Update the listbox and preview
  tkdelete(listbox_cols, 0, "end")
  for (colname in names(df)) {
    tkinsert(listbox_cols, "end", colname)
  }

  refresh_preview()
  update_selected_columns()
}

# Hash columns
hash_columns <- function() {
  if (is.null(my_data)) {
    tkmessageBox(message = "No data loaded.", icon = "warning")
    return()
  }

  sel_indices <- as.integer(tkcurselection(listbox_cols))
  if (length(sel_indices) < 1) {
    tkmessageBox(message = "Please select at least one column.", icon = "warning")
    return()
  }

  df <- my_data
  selected_colnames <- names(df)[sel_indices + 1]  # Adjust for 0-based indexing

  # Concatenate and hash selected columns
  df$HASH <- sapply(seq_len(nrow(df)), function(i) {
    combined_text <- paste0(df[i, selected_colnames], collapse = "#")
    generate_hashed_substrings_with_offsets(combined_text)
  })

  # Remove original columns and update data
  df <- df[, !(names(df) %in% selected_colnames)]
  assign("my_data", df, envir = .GlobalEnv)

  # Refresh listbox and preview
  tkdelete(listbox_cols, 0, "end")
  for (colname in names(df)) {
    tkinsert(listbox_cols, "end", colname)
  }

  refresh_preview()
  update_selected_columns()
}

# Save file as CSV or XLSX
save_file <- function() {
  if (is.null(my_data)) {
    tkmessageBox(message = "No data to save.", icon = "warning")
    return()
  }

  filetypes_str <- '{"CSV Files" ".csv"} {"Excel Files" ".xlsx"} {"All Files" "*"}'

  fpath <- tclvalue(tkgetSaveFile(
    filetypes = filetypes_str,
    defaultextension = ".csv"
  ))

  if (!nzchar(fpath)) return()

  ext <- tolower(tools::file_ext(fpath))

  if (ext == "csv") {
    write.csv(my_data, fpath, row.names = FALSE)
    tkmessageBox(message = paste("Data saved to CSV:", fpath))
  } else if (ext == "xlsx") {
    openxlsx::write.xlsx(my_data, fpath)
    tkmessageBox(message = paste("Data saved to XLSX:", fpath))
  } else {
    write.csv(my_data, fpath, row.names = FALSE)
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

  # Frames
  frame_buttons <- tkframe(my_win)
  frame_listbox <- tkframe(my_win)
  frame_preview <- tkframe(my_win)

  # Pack frames
  tkpack(frame_buttons, side = "top", fill = "x", padx = 10, pady = 5)
  tkpack(frame_listbox, side = "left", fill = "y", padx = 10, pady = 5)
  tkpack(frame_preview, side = "right", fill = "both", expand = TRUE, padx = 10, pady = 5)

  # Buttons
  btn_load <- tkbutton(frame_buttons, text = "Load File", command = load_file)
  btn_hash <- tkbutton(frame_buttons, text = "Hash Columns", command = hash_columns)
  btn_save <- tkbutton(frame_buttons, text = "Save File", command = save_file)
  tkpack(btn_load, btn_hash, btn_save, side = "left", padx = 5)

  # Listbox for column selection with scrollbar
  lbl_cols <- tklabel(frame_listbox, text = "Select columns:")
  scrollbar_listbox <- tkscrollbar(frame_listbox, orient = "vertical", command = function(...) tkyview(listbox_cols, ...))
  listbox_cols <- tklistbox(frame_listbox, selectmode = "multiple", width = 20, height = 15, yscrollcommand = function(...) tkset(scrollbar_listbox, ...))
  tkpack(lbl_cols, side = "top")
  tkpack(listbox_cols, side = "left", fill = "y")
  tkpack(scrollbar_listbox, side = "right", fill = "y")

  # Text preview area with both scrollbars
  lbl_preview <- tklabel(frame_preview, text = "Data Preview (first 10 rows):")
  scrollbar_preview_y <- tkscrollbar(frame_preview, orient = "vertical", command = function(...) tkyview(preview_text, ...))
  scrollbar_preview_x <- tkscrollbar(frame_preview, orient = "horizontal", command = function(...) tkxview(preview_text, ...))
  preview_text <- tktext(frame_preview, wrap = "none", width = 80, height = 20, yscrollcommand = function(...) tkset(scrollbar_preview_y, ...), xscrollcommand = function(...) tkset(scrollbar_preview_x, ...))
  tkpack(lbl_preview, side = "top", anchor = "w")
  tkpack(preview_text, side = "top", fill = "both", expand = TRUE)
  tkpack(scrollbar_preview_y, side = "right", fill = "y")
  tkpack(scrollbar_preview_x, side = "bottom", fill = "x")

  # Label for selected columns
  selected_columns_label <- tklabel(frame_preview, text = "Selected columns: None", anchor = "w", wraplength = 600)
  tkpack(selected_columns_label, side = "top", pady = 5)

  # Deiconify and start event loop
  tkwm.deiconify(my_win)
  tkfocus(my_win)
  tkwait.window(my_win)
}
