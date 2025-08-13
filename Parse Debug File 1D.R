library(data.table)

# Define a function to parse a single packet block
parse_packet_block <- function(block) {
  # Initialize a list to hold the parsed data
  data_list <- list()
  
  # Extract timestamp and type
  header <- trimws(strsplit(block[1], "\\[")[[1]])
  data_list$timestamp <- trimws(header[1])
  data_list$type <- gsub("\\]", "", trimws(header[2]))
  
  # Split the block into lines and process each line
  lines <- block[-1]
  for (line in lines) {
    if (grepl(":", line)) {
      # Handle key-value pairs
      parts <- trimws(strsplit(line, ":")[[1]])
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      
      # Clean up key names for R
      key <- gsub(" ", "_", key)
      key <- gsub("\\{|\\}", "", key)
      
      # Handle nested 'decoded' block
      if (key == "decoded") {
        next_line_index <- which(lines == line) + 1
        while (next_line_index <= length(lines) && !grepl(":", lines[next_line_index])) {
          sub_line <- trimws(lines[next_line_index])
          if (grepl("portnum", sub_line)) {
            data_list$portnum <- trimws(strsplit(sub_line, ":")[[1]][2])
          }
          if (grepl("payload", sub_line)) {
            data_list$payload <- trimws(strsplit(sub_line, ":")[[1]][2])
          }
          if (grepl("bitfield", sub_line)) {
            data_list$bitfield <- as.integer(trimws(strsplit(sub_line, ":")[[1]][2]))
          }
          if (grepl("want_response", sub_line)) {
            data_list$want_response <- as.logical(trimws(strsplit(sub_line, ":")[[1]][2]))
          }
          next_line_index <- next_line_index + 1
        }
      } else {
        # Store other key-value pairs
        # Try to convert to numeric if possible
        if (!is.na(suppressWarnings(as.numeric(value)))) {
          data_list[[key]] <- as.numeric(value)
        } else {
          data_list[[key]] <- value
        }
      }
    }
  }
  
  return(data.table(data_list))
}

# --- Main Script ---
# Step 1: Read the entire file as a single string
debug_file_path <- "meshtastic_debug.log" # Replace with your file path
# Create a temporary file to demonstrate the script
temp_file <- tempfile(fileext = ".log")
writeLines(
  c("7/15/25 10:30:42 PM [Packet]",
    "from: 3711455628 (!dd38518c)",
    "to: 4294967295 (!ffffffff)",
    "decoded {",
    "  portnum: TELEMETRY_APP",
    "  payload: \"\\r\\356*wh\\022\\025\\be\\025q=\\206@\\0357\\320\\320A%Jy\\313?(\\310\\246\\006\"",
    "}",
    "id: 727577395",
    "rx_time: 1752640238",
    "hop_limit: 3",
    "priority: BACKGROUND",
    "",
    "",
    "7/15/25 10:30:39 PM [Packet]",
    "from: 2733364712 (!a2ebd5e8)",
    "to: 4294967295 (!ffffffff)",
    "decoded {",
    "  portnum: POSITION_APP",
    "  payload: \"\\r\\000\\000<\\025\\025\\000\\000\\314\\300\\030\\210\\016%\\325*wh(\\001x\\000\\200\\001\\000\\270\\001\\r\"",
    "  bitfield: 1",
    "}",
    "id: 2748032179",
    "rx_time: 1752640235",
    "rx_snr: -5.5",
    "hop_limit: 2",
    "rx_rssi: -112",
    "hop_start: 7"
  ),
  temp_file
)
debug_file_path <- temp_file
lines <- readLines(debug_file_path)

# Step 2: Split the file content into individual packet blocks
# Each block starts with a date/time stamp
block_indices <- grep("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{2}:[0-9]{2} (AM|PM)", lines)
packet_blocks <- list()
for (i in seq_along(block_indices)) {
  start_index <- block_indices[i]
  end_index <- if (i < length(block_indices)) {
    block_indices[i+1] - 1
  } else {
    length(lines)
  }
  packet_blocks[[i]] <- lines[start_index:end_index]
}

# Step 3: Apply the parsing function to each block
parsed_data_list <- lapply(packet_blocks, parse_packet_block)

# Step 4: Combine the list of data.tables into a single data.table
final_dt <- rbindlist(parsed_data_list, fill = TRUE)

# Step 5: Convert timestamp and rx_time to appropriate formats
# This is the corrected line using `as.character()` to prevent the "closure" error.
# final_dt[, timestamp := as.POSIXct(as.character(timestamp), format = "%m/%d/%y %I:%M:%S %p")]
# final_dt[, rx_time := as.POSIXct(rx_time, origin = "1970-01-01")]

# Print the resulting data.table
print(final_dt)

# Clean up the temporary file
unlink(temp_file)
