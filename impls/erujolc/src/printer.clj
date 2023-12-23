(ns printer)

(defprotocol MalPrinter
  (mal-print-string [this print-readably]
    "Return the value of 'this as a string (for atomic types).")
  (mal-print-list [this print-readably]
    "Return the value of 'this as a string (for collection types)."))
