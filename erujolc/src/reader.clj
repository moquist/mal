(ns reader
  (:require types))

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
          (types/->MalDatum type coll)]) ; TODO: implement :map as a Clojure map?
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

  Return vector: ['reader atom]

  Example call:
  (read-atom (-> \"(a b c)\" tokenize (->MalReader 0) mal-step))"
  [reader]
  (let [[reader tok] (mal-next reader)
        datum (cond
                (= tok "nil") (types/->MalDatum :nil nil)
                (= tok "false") (types/->MalDatum :bool false)
                (= tok "true") (types/->MalDatum :bool true)
                (-> tok first (= \:)) (types/->MalDatum :keyword (str tok))
                (re-matches #"\d+" tok) (types/->MalDatum :int (Integer. tok))
                ;; split out :string handling to catch malformed strings, e.g.: "abc
                (= \" (first tok) (last tok)) (types/->MalDatum :string (tok->str tok))
                :else (types/->MalDatum :symbol (symbol tok)))]
    [reader datum]))

(defn read-form
  "Peek at the first token, and handle list or atom reading.

  Return vector: ['reader result]

  Example call:
  (-> \"(a b c)\" tokenize (->MalReader 0) read-form) "
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
