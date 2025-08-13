library(data.table)

# Define a function to parse a single packet block
parse_packet_block <- function(block) {
  # Initialize a list to hold the parsed data
  data_list <- list()
  
  # Extract timestamp and packet type from the first line
  header <- trimws(strsplit(block[1], "\\[")[[1]])
  data_list$timestamp <- trimws(header[1])
  
  # Process the remaining lines in the block
  lines <- block[-1]
  for (line in lines) {
    if (grepl(":", line)) {
      # Use a regular expression to handle lines with complex values
      # like "from: 3711455628 (!dd38518c)"
      parts <- trimws(strsplit(line, ":")[[1]])
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = ":"))
      
      # Clean up key names
      key <- gsub(" ", "_", key)
      key <- gsub("\\{|\\}|\\!|\\(|\\)", "", key)
      
      # Handle nested 'decoded' block
      if (key == "decoded") {
        # Extract 'portnum' from the decoded section
        decoded_line <- grep("portnum:", lines, value = TRUE)
        if (length(decoded_line) > 0) {
          data_list$portnum <- trimws(strsplit(decoded_line, ":")[[1]][2])
        }
        # The payload is complex, we will not parse it further here
      } else {
        # Store other key-value pairs, converting to numeric where possible
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
# Step 1: Read the entire file
debug_file_path <- "meshtastic_debug.log" # Replace with your file path
# Create a temporary file with your example data for demonstration
temp_file <- tempfile(fileext = ".log")
writeLines(
  c("7/15/25 10:30:42 PM [Packet]",
    "from: 3711455628 (!dd38518c)",
    "to: 4294967295 (!ffffffff)",
    "decoded {",
    "  portnum: TELEMETRY_APP",
    "  payload: \"...\"",
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
    "  payload: \"...\"",
    "  bitfield: 1",
    "}",
    "id: 2748032179",
    "rx_time: 1752640235",
    "rx_snr: -5.5",
    "hop_limit: 2",
    "rx_rssi: -112",
    "hop_start: 7",
    "",
    "",
    "7/15/25 10:30:31 PM [Packet]",
    "from: 3736247704 (!deb29d98)",
    "to: 4294967295 (!ffffffff)",
    "decoded {",
    "  portnum: POSITION_APP",
    "  payload: \"...\"",
    "  bitfield: 0",
    "}",
    "id: 4206438195",
    "rx_time: 1752640227",
    "rx_snr: -6.25",
    "rx_rssi: -114",
    "hop_start: 3",
    "",
    "",
    "7/15/25 10:30:24 PM [Packet]",
    "from: 1129934056 (!43596ce8)",
    "to: 1129810496 (!43578a40)",
    "decoded {",
    "  portnum: NODEINFO_APP",
    "  payload: \"...\"",
    "  want_response: true",
    "}",
    "id: 2601402384",
    "rx_time: 1752640216",
    "rx_snr: -17.25",
    "hop_limit: 2",
    "rx_rssi: -125",
    "hop_start: 7"
  ),
  temp_file
)
debug_file_path <- temp_file
lines <- readLines(debug_file_path)

# Step 2: Split the file into individual packet blocks
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

# Step 3: Parse each block and combine into a data.table
parsed_data_list <- lapply(packet_blocks, parse_packet_block)
final_dt <- rbindlist(parsed_data_list, fill = TRUE)

# Step 4: Convert timestamp and rx_time to the correct formats
#final_dt[, timestamp := as.POSIXct(as.character(timestamp), format = "%m/%d/%y %I:%M:%S %p")]
#final_dt[, rx_time := as.POSIXct(rx_time, origin = "1970-01-01")]
timestamp <- "timestamp holder"
rx_time <- "rx_time holder"

# Step 5: Select and reorder columns for a clean 2D table
# Define the columns you are interested in
columns_of_interest <- c(
  "timestamp", "from", "to", "portnum", "id", 
  "rx_snr", "rx_rssi", "hop_limit", "hop_start", "priority", 
  "bitfield", "want_response"
)

# Filter the list to include only columns that exist in the data.table
# This is the key correction.
existing_columns <- intersect(columns_of_interest, names(final_dt))

# Select and reorder the existing columns
final_dt <- final_dt[, ..existing_columns]

# Print the resulting data.table
print(final_dt)

# Clean up the temporary file
unlink(temp_file)