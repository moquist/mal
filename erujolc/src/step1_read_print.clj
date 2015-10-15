(ns step1-read-print
  (:require utils))

;; ========================================
;; Printer

(defprotocol MalPrinter
  (mal-print-string [this] "Return the value of 'this as a string (for atomic types).")
  (mal-print-list [this] "Return the value of 'this as a string (for collection types)."))

;; ========================================
;; Mal types

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
(def mal-types #{:list :vector :string :keyword :map :nil :int :fn})

(defrecord MalDatum [type val]
  MalPrinter
  (mal-print-string [this]
    (condp = type
      :fn "#<function>"
      :keyword val
      :list (str "(" (mal-print-list this) ")")
      :vector (str "[" (mal-print-list this) "]")
      :string (str \" val \")
      :map (str "{" (mal-print-list this) "}")
      :nil "nil"
      (str val)))
  (mal-print-list [_]
    (->> val
         (map mal-print-string)
         (interpose " ")
         (apply str))))

;; ========================================
;; reader
(defn tokenize
  "Split input string into Mal tokens.
  Regex taken from Mal step instructions."
  [code]
  (re-seq #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
          code))

(defprotocol MalRead
  (mal-next [this]
    "return vector: [this tok]
         this => this, with 'position incremented
         tok  => token from 'mal-peek before increment")
  (mal-peek [this]
    "return tok from current 'position")
  (mal-step [this]
    "increment 'position, return this"))

(defrecord MalReader [tokens position]
  MalRead
  (mal-step [this]
    (update-in this [:position] inc))
  (mal-peek
    [this]
    (let [[_orig-string tok] (nth tokens position)]
      tok))
  (mal-next
    [this]
    [(mal-step this)
     (mal-peek this)]))

(declare read-form)
(defn read-coll
  "Read forms from 'reader into a collection of type 'type until the
  current form closes.

  Return vector: ['reader collection] with 'reader stepped past the
  closing token of the current form.

  Example call:
  (read-coll :list (-> \"(a b c)\" tokenize (->MalReader 0) mal-step))"
  ([type reader] (read-coll type reader []))

  ([type reader coll]
   (let [closer (condp = type
                  :list ")"
                  :vector "]"
                  :map "}")]
     (if (= closer
            (try (mal-peek reader)
                 (catch IndexOutOfBoundsException _
                   (println (str "expected '" closer "', got EOF"))
                   (throw (Exception. "BadForm")))))
       (do
         (utils/debug :read-coll :closing :type type :reader reader :coll coll)
         [(mal-step reader) ; step over closing token
          (->MalDatum type coll)]) ; TODO: implement :map as a Clojure map?
       (let [[reader result] (read-form reader)]
         (utils/debug :read-coll :reader reader :result result)
         (recur type reader (conj coll result)))))))

(defn tok->str
  "Take a quoted string-literal token.
  Remove opening and closing quotes.
  Escape internal double-quotes and newlines.
  Return the parsed string-literal."
  [tok]
  (-> (->> tok
           rest
           butlast
           (apply str))
      (clojure.string/replace #"\\\"" "\"")
      (clojure.string/replace #"\\n" "\n")))

(defn read-atom
  "Read an atom.

  Return vector: ['reader atom]"
  [reader]
  (let [[reader tok] (mal-next reader)
        datum (cond
                (= tok "nil") (->MalDatum :nil nil)
                (= tok "false") (->MalDatum :bool false)
                (= tok "true") (->MalDatum :bool true)
                (-> tok first (= \:)) (->MalDatum :keyword (str tok))
                (re-matches #"\d+" tok) (->MalDatum :int (Integer. tok))
                ;; split out :string handling to catch malformed strings, e.g.: "abc
                (= \" (first tok) (last tok)) (->MalDatum :string (tok->str tok))
                :else (->MalDatum :symbol (symbol tok)))]
    [reader datum]))

(defn read-form
  "Peek at the first token, and handle list or atom reading.

  Return vector: ['reader result]"
  [reader]
  (let [tok (mal-peek reader)]
    (condp = tok
      "(" (read-coll :list (mal-step reader))
      "[" (read-coll :vector (mal-step reader))
      "{" (read-coll :map (mal-step reader))
      (read-atom reader))))

(defn mal-read-string
  "Tokenize input string, and then call 'read-form on tokens."
  [mal-code]
  (-> mal-code
      tokenize
      (->MalReader 0)
      read-form))

;; ========================================
;; REPL
(defn READ [x] (mal-read-string x))
(defn EVAL [x] x)
(defn PRINT [x] (mal-print-string x))
(defn rep [x]
  (-> x
      READ
      second
      EVAL
      PRINT))

(defn prompt []
  (print "user> ")
  (flush)
  (read-line))

(defn -main []
  (if-let [x (prompt)]
    (do
      (try
        (println (rep x))
        (catch Exception _))
      (recur))))
