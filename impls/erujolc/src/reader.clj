(ns reader
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            types
            utils))

(defn tokenize
  "Split input string into Mal tokens.
  Regex taken from Mal step instructions.
  Returns a seq of vectors containing [original-string parsed-token]"
  [code]
  (->> (re-seq
         ;; old: #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
                 #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"
          code)
       (remove #(-> % second (= "")))))

(comment
  (tokenize "(+ 3 7)")
  (tokenize "    (   + 3 7   )    ")
  )

(utils/defprotocol-once MalRead
  (-mal-next [this]
    "return vector: [this tok]
         this => this, with 'position incremented
         tok  => token from 'mal-peek before increment")
  (-mal-peek [this]
    "return tok from current 'position")
  (-mal-step [this]
    "increment 'position, return this"))

(defn mal-next [x] (-mal-next x))
(defn mal-peek [x] (-mal-peek x))
(defn mal-step [x] (-mal-step x))

(defrecord MalReader [tokens position]
  MalRead
  (-mal-step [this]
    (update this :position inc))
  (-mal-peek
    [_]
    (utils/debug :MalReader/mal-peek :count-tokens (count tokens) :position position)
    (if (<= (count tokens) position)
      (do
        (utils/debug :MalReader/mal-peek :returning ::peeked-into-the-abyss)
        ::peeked-into-the-abyss)
      (do
        (utils/debug :MalReader/mal-peek :returning (-> tokens (nth position) second))
        (-> tokens (nth position) second))))
  (-mal-next
    [this]
    [(-mal-step this)
     ;;        ^v-- Immutable data, in case you forgot.
     (-mal-peek this)]))

(declare read-form)
(defn read-coll
  "Read forms from 'reader into a collection of type 'typ until the
  current form closes.

  Return vector: ['reader collection] with 'reader stepped past the
  closing token of the current form.

  Example call:
  (read-coll :list (-> \"(a b c)\" tokenize (->MalReader 0) mal-step))"
  ([typ reader] (read-coll typ reader (condp = typ
                                        :list []
                                        :map {}
                                        :vector [])))
  ([typ reader coll]
   (let [[paren-plural closer] (condp = typ
                                 :list ["parens" ")"]
                                 :vector ["brackets" "]"]
                                 :map ["braces" "}"])
         next-tok (mal-peek reader)]
     (utils/debug :read-coll :next-tok next-tok)
     (when (= next-tok ::peeked-into-the-abyss)
       (println (str "unbalanced " paren-plural))
       (throw (ex-info "unbalanced form" {:cause :unclosed-form :reader reader})))
     (if (= closer next-tok)
       ;; step over closing token
       (let [stepped-reader (mal-step reader)]
         (utils/debug :read-coll :pre-validation :typ typ :reader reader :coll coll)
         [stepped-reader (types/->MalDatum typ coll)])
       (condp = typ
         :list (let [[reader result] (read-form reader)]
                 (utils/debug :read-coll :reader reader :result result)
                 (recur typ reader (conj coll result)))
         :map (let [[reader k] (read-form reader)
                    [reader v] (read-form reader)]
                 (utils/debug :read-coll :reader reader :k k :v v)
                 (recur typ reader (assoc coll k v)))
         :vector (let [[reader result] (read-form reader)]
                   (utils/debug :read-coll :reader reader :result result)
                   (recur typ reader (conj coll result)))
         )
       ))))

#_
(defn wrap-read [sym reader]
  (let [[reader form] (read-form reader)]
    [reader ]
    ))

(comment
  (println (edn/read-string "\"abc\""))
  (edn/read-string "\"")
  )
(defn tok->str
  "Take a quoted string-literal token.
  If incorrectly quoted, throw.
  Else return the parsed string-literal."
  [reader tok]
  (utils/debug ::tok->str :tok tok)
  (let [x (try (edn/read-string tok)
               (catch Exception e
                 (let [e-str (.toString e)]
                   (if (str/includes? e-str "EOF while reading string")
                     (throw (ex-info e-str
                                     {:cause :eof-while-reading-string
                                      :reader reader}))
                     (throw e)))))]
    (utils/debug ::tok-str :x x)
    x
    ))

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
                (-> tok first (= \:)) (types/->MalDatum :keyword (keyword (subs tok 1)))
                (re-matches #"\d+" tok) (types/->MalDatum :int (Integer. tok))
                ;; split out :string handling to catch malformed strings, e.g.: "abc
                (= \" (first tok)) (types/->MalDatum :string (tok->str reader tok))
                :else (do
                        (utils/debug ::read-atom :symbol-tok :typ-tok (type tok) :tok tok)
                        (types/->MalDatum :symbol (symbol tok))))]
    [reader datum]))

(comment
  (-> "(a b c)" tokenize (->MalReader 0) read-form)
  (-> "\"abc\"" tokenize #_(->MalReader 0) #_read-form)
  )


(defn read-form
  "Peek at the first token, and handle list or atom reading.

  Return vector: ['reader result]

  Example call:
  (-> \"(a b c)\" tokenize (->MalReader 0) read-form) "
  [reader]
  (let [tok (mal-peek reader)]
    (utils/debug ::read-form :tok tok :equal? (= ::peeked-into-the-abyss tok))
    (cond
      (= ::peeked-into-the-abyss tok) (do (utils/debug ::read-form :tok2 tok)
                                          [reader tok]) ; done!
      (= \; (first tok)) "" ; comment!
      :default (do (utils/debug ::read-form :tok3 tok)
                   (condp = tok
                     "(" (read-coll :list (mal-step reader))
                     "[" (read-coll :vector (mal-step reader))
                     "{" (read-coll :map (mal-step reader))
                     #_#_
                     "'" (wrap-read 'quote (mal-step reader))
                     (read-atom reader))))))

(defn read-forms
  "Like read-form, but keeps reading until we've ::peeked-into-the-abyss."
  [reader]
  (loop [[reader form] (read-form reader)
         ;; could track :position here for error messages
         forms []]
    ;; TODO: can't read all at once, must REPL *each* form independently
    (utils/debug ::read-forms :form form :forms forms)
    (if (not= ::peeked-into-the-abyss form)
      (recur (read-form reader) (conj forms form))
      [reader forms])))

(defn mal-read-string
  "Tokenize input string, and then call 'read-forms on tokens.
   Return: [reader MalDatum]"
  [mal-code]
  (-> mal-code
      tokenize
      (->MalReader 0)
      read-form))
