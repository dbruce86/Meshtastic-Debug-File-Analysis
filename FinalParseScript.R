library(data.table)

# Define a function to parse a single packet block
parse_packet_block <- function(block) {
  # print("--- Starting parse_packet_block function ---")
  # print(paste("Received block string:", block))
  
  # Initialize a list to hold the parsed data for a single packet
  packet_data <- list()
  
  # Split the block into lines
  lines <- strsplit(block, "\n")[[1]]
  # print(paste("Block split into", length(lines), "lines."))
  
  # Process each line
  for (line in lines) {
    line <- trimws(line)
    
    if (line == "") {
      next # Skip empty lines
    }
    
    # Extract key-value pairs
    if (grepl(":", line)) {
      parts <- trimws(strsplit(line, ":")[[1]])
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = ":"))
      
      # Clean up key names
      key <- gsub(" ", "_", key)
      key <- gsub("\\{|\\}|\\!|\\(|\\)", "", key)
      
      # print(paste("Extracting key:", key, "with value:", value))
      
      # Try to convert value to numeric
      if (!is.na(suppressWarnings(as.numeric(value)))) {
        packet_data[[key]] <- as.numeric(value)
      } else {
        packet_data[[key]] <- value
      }
    }
  }
  
  # Return a single-row data.table for this packet
  if (length(packet_data) > 0) {
    # CRITICAL DIAGNOSTIC: Print the list before it's converted to a data.table
    # print("Packet data list before conversion:")
    # str(packet_data)
    
    # This is the corrected line:
    result <- as.data.table(packet_data)
    # print("Returning a data.table with these columns:")
    # print(names(result))
    return(result)
  } else {
    # print("No data collected for this packet. Returning NULL.")
    return(NULL)
  }
}

# --- Main Script ---

# A confirmation print statement
# print("--- Running the final, corrected Meshtastic parsing script... ---")

# Step 1: Define the log data as a character vector with dates removed
log_data <- c(
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
)

# Step 2: Manually parse the data and build packets
parsed_data_list <- list()
current_block_lines <- c()
for (line in log_data) {
  if (line == "" && length(current_block_lines) > 0) {
    parsed_data_list <- c(parsed_data_list, list(parse_packet_block(paste(current_block_lines, collapse = "\n"))))
    current_block_lines <- c()
  } else if (line != "") {
    current_block_lines <- c(current_block_lines, line)
  }
}
# Add the last packet block if it exists
if (length(current_block_lines) > 0) {
  parsed_data_list <- c(parsed_data_list, list(parse_packet_block(paste(current_block_lines, collapse = "\n"))))
}

# A crucial print to see the full list of data tables before combining
# print("--- Full list of parsed data tables before rbindlist ---")
# str(parsed_data_list)

# Step 3: Combine into a data.table
final_dt <- rbindlist(parsed_data_list, fill = TRUE)

# print("--- Data table successfully created ---")

# Step 4: Convert rx_time to the correct format
if ("rx_time" %in% names(final_dt)) {
  final_dt[, rx_time := as.POSIXct(rx_time, origin = "1970-01-01")]
}

# Step 5: Select and reorder columns for a clean 2D table
columns_of_interest <- c(
  "from", "to", "portnum", "id",
  "rx_snr", "rx_rssi", "hop_limit", "hop_start", "priority",
  "bitfield", "want_response", "rx_time"
)
existing_columns <- intersect(columns_of_interest, names(final_dt))
final_dt <- final_dt[, ..existing_columns]

# Print the resulting data.table
print(final_dt)