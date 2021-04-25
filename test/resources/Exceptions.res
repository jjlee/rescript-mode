// Obviously ideally this would be indented two spaces not four, have not
// investigated...

let getItem = (items) =>
    if callSomeFunctionThatThrows() {
      // return the found item here
      1
    } else {
      raise(Not_found)
    }

let result =
  try {
    getItem([1, 2, 3])
  } catch {
      | Not_found => 0 // Default value if getItem throws
  }
