(ns printer)

(defprotocol MalPrinter
  (-mal-print-string [this print-readably]
    "Return the value of 'this as a string (for atomic types).")
  (-mal-print-list [this print-readably]
    "Return the value of 'this as a string (for collection types).")
  (-mal-print-map [this print-readably]
    "Return the value of 'this as a string (for maps)"))

(defn mal-print-string [this print-readably]
  (-mal-print-string this print-readably ))

(defn mal-print-list [this print-readably]
  (-mal-print-list this print-readably ))

(defn mal-print-map [this print-readably]
  (-mal-print-map this print-readably ))
