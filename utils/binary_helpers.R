dectobin <- function(x) {
  map_chr(x, function(x) {
    intToBits(x) %>%
      rev %>%
      as.integer %>%
      paste(collapse = '') %>%
      as.integer %>%
      as.character
  })
}

# Function to convert an integer to 32-bit binary representation
int_to_binary_32bit <- function(num) {
  if (!is.numeric(num) || floor(num) != num) {
    stop("Input must be an integer.")
  }
  
  if (num < -2^31 || num >= 2^31) {
    stop("Input must fit within a 32-bit signed integer range (-2147483648 to 2147483647).")
  }
  
  # Convert the integer to binary
  binary <- as.integer(intToBits(num))
  
  # Extract the first 32 bits and reverse to match human-readable binary order
  binary_32bit <- rev(binary[1:32])
  
  # Collapse the binary vector into a single string
  paste(binary_32bit, collapse = "")
}

# Example usage
# int_to_binary_32bit(5)         # Positive integer
# int_to_binary_32bit(-5)        # Negative integer
# int_to_binary_32bit(2147483647) # Largest 32-bit signed integer
# int_to_binary_32bit(-2147483648) # Smallest 32-bit signed integer

# Function to convert a 32-bit binary string to an integer
binary_32bit_to_int <- function(binary) {
  # Ensure input is a valid 32-bit binary string
  if (!is.character(binary) || nchar(binary) != 32 || !all(strsplit(binary, "")[[1]] %in% c("0", "1"))) {
    stop("Input must be a 32-bit binary string.")
  }
  
  # Convert the binary string to a vector of digits
  binary_vec <- as.numeric(strsplit(binary, "")[[1]])
  
  # Reverse the vector to match the order of `intToBits` (least significant bit first)
  binary_vec <- rev(binary_vec)
  
  # Calculate the integer value
  # Handle signed integers (two's complement)
  if (binary_vec[32] == 1) {
    # If the sign bit is 1, it's negative
    # Compute the two's complement by flipping bits and adding 1
    flipped_bits <- 1 - binary_vec  # Flip the bits
    int_value <- sum(flipped_bits * 2^(0:31)) + 1  # Add 1
    int_value <- -int_value  # Negate for negative number
  } else {
    # Positive number
    int_value <- sum(binary_vec * 2^(0:31))
  }
  
  return(int_value)
}

# Example usage
# binary_32bit_to_int("00000000000000000000000000000101")    # Positive integer
# binary_32bit_to_int("11111111111111111111111111111011")    # Negative integer
# binary_32bit_to_int("01111111111111111111111111111111")    # Largest positive 32-bit integer
# binary_32bit_to_int("10000000000000000000000000000000")    # Smallest negative 32-bit integer


