(ns printer)

(defprotocol MalPrinter
  (mal-print-string [this] "Return the value of 'this as a string (for atomic types).")
  (mal-print-list [this] "Return the value of 'this as a string (for collection types)."))

