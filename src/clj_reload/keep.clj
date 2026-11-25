(ns clj-reload.keep
  (:require
    [clj-reload.util :as util]
    [clojure.string :as str])
  (:import
    [clojure.lang LineNumberingPushbackReader Var]
    [java.io File]))

(defn maybe-quote [form]
  (cond
    (symbol? form)
    (list 'quote form)
   
    (and (sequential? form) (not (vector? form)))
    (list 'quote form)
   
    :else
    form))

(defn meta-str [var]
  (let [meta (dissoc (meta var) :ns :file :line :column :name)]
    (when-not (empty? meta)
      (str "^" (pr-str (util/map-vals meta maybe-quote)) " "))))

(defn classname [ns sym]
  (-> (name ns)
    (str/replace "-" "_")
    (str "." sym)))

(defn stash-ns []
  (or
    (find-ns 'clj-reload.stash)
    (binding [*ns* *ns*]
      (in-ns 'clj-reload.stash)
      *ns*)))

(def keep-methods-defs
  {:resolve 
   (fn [ns sym keep]
     (when-some [var (resolve (symbol (name ns) (name sym)))]
       {:var var}))
   
   :patch
   (fn [ns sym keep]
     (when-some [var (:var keep)]
       (intern (stash-ns) sym @var)
       (str
         "(def " (meta-str var) sym " @#'clj-reload.stash/" sym ")")))})

(def keep-methods-deftype
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       {:ctor ctor}))
   
   :patch
   (fn [ns sym keep]
     (when-some [ctor (:ctor keep)]
       (intern (stash-ns) (symbol (str "->" sym)) @ctor)
       (str
         "(clojure.core/import " (classname ns sym) ") "
         "(def " (meta-str ctor) "->" sym " clj-reload.stash/->" sym ")")))})

(def keep-methods-defrecord
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       (when-some [map-ctor (resolve (symbol (name ns) (str "map->" sym)))]
         {:ctor ctor
          :map-ctor map-ctor})))
   
   :patch
   (fn [ns sym keep]
     (when-some [ctor (:ctor keep)]
       (when-some [map-ctor (:map-ctor keep)]
         (intern (stash-ns) (symbol (str "->" sym)) @ctor)
         (intern (stash-ns) (symbol (str "map->" sym)) @map-ctor)
         (str
           "(clojure.core/import " (classname ns sym) ") "
           "(def " (meta-str ctor) "->" sym " clj-reload.stash/->" sym ") "
           "(def " (meta-str map-ctor) "map->" sym " clj-reload.stash/map->" sym ")"))))})

(defn update-protocol-method-builders [proto & vars]
  (let [mb   (:method-builders proto)
        vars (util/for-map [var vars]
               [(symbol var) var])
        mb'  (util/for-map [[var val] mb]
               [(vars (symbol var)) val])]
    (alter-var-root (:var proto) assoc :method-builders mb')))

(def keep-methods-defprotocol
  {:resolve 
   (fn [ns sym keep]
     (when-some [proto (resolve (symbol (name ns) (name sym)))]
       {:proto   proto
        :methods (util/for-map [[method-var _] (:method-builders @proto)]
                   [(.-sym ^Var method-var) method-var])}))
   
   :patch
   (fn [ns sym keep]
     (when-some [proto (:proto keep)]
       (when-some [methods (:methods keep)]
         (intern (stash-ns) sym @proto)
         (doseq [[method-sym method] methods]
           (intern (find-ns 'clj-reload.stash) method-sym @method))
         (str
           "(def " (meta-str proto) sym " clj-reload.stash/" sym ") "
           "(clojure.core/alter-var-root #'" sym " assoc :var #'" sym ") "
           (str/join " "
             (for [[method-sym method] methods]
               (str "(def " (meta-str method) "^{:protocol #'" sym "} " method-sym " clj-reload.stash/" method-sym ")")))
           " (clj-reload.keep/update-protocol-method-builders " sym " "
           (str/join " "
             (for [[method-sym _] methods]
               (str "#'" method-sym))) ") "
           ; "(-reset-methods " sym ")"
           ))))})

(def keep-methods
  (delay
    @(resolve 'clj-reload.core/keep-methods)))

(defn keep-resolve [ns sym keep]
  ((:resolve (@keep-methods (:tag keep))) ns sym keep))

(defn keep-patch [ns sym keep]
  ((:patch (@keep-methods (:tag keep))) ns sym keep))

(defn resolve-keeps [ns syms]
  (util/for-map [[sym keep] syms
                 :let [resolved (keep-resolve ns sym keep)]
                 :when resolved]
    [sym resolved]))

(defn- line-offsets
  "Returns a vector of the character offset at which each line of `content`
  starts, indexed from zero."
  [^String content]
  (loop [i 0, acc (transient [0])]
    (let [j (.indexOf content "\n" (int i))]
      (if (neg? j)
        (persistent! acc)
        (recur (inc j) (conj! acc (inc j)))))))

(defn- reader-offset
  "Character offset of `rdr` into the content it reads, computed from its
  line/column position. Jolt's reader lacks `captureString`/`getString`."
  ^long [offsets ^String content ^LineNumberingPushbackReader rdr]
  (let [line (dec (.getLineNumber rdr))]
    ;; at EOF jolt reports a phantom extra line, even without a trailing newline
    (if (< line (count offsets))
      (min (count content)
           (+ (nth offsets line) (dec (.getColumnNumber rdr))))
      (count content))))

(defn- patch-file* [^String content patch-fn]
  (let [rdr     (util/string-reader content)
        offsets (line-offsets content)
        patched (StringBuilder.)]
    (loop [start 0]
      (let [form (util/read-form rdr)
            end  (reader-offset offsets content rdr)
            text (subs content start end)]
        (cond
          (= :clj-reload.util/eof form)
          (str patched)
          
          (and (list? form) (>= (count form) 2))
          (if-some [text' (patch-fn (take 2 form))]
            (let [[_ ws] (re-matches #"(?s)(\s*).*" text)
                  _      (.append patched ws)
                  text   (subs text (count ws))
                  lines  (str/split-lines text)
                  text'' (if (= 1 (count lines))
                           (if (<= (count text) (count text'))
                             text'
                             (str text' (str/join (repeat (- (count text) (count text')) \space))))
                           (str text'
                             (str/join (repeat (dec (count lines)) \newline))
                             (str/join (repeat (count (last lines)) \space))))]
              (do
                (.append patched text'')
                (recur end)))
            (do
              (.append patched text)
              (recur end)))
          
          :else
          (do
            (.append patched text)
            (recur end)))))))

(defn patch-file [content patch-fn]
  (util/with-read-ns content
    (patch-file* content patch-fn)))

(defn patch-fn [ns keeps]
  (fn [[tag sym]]
    (when-some [keep (get keeps sym)]
      (when (= tag (:tag keep))
        (keep-patch ns sym keep)))))

(defn ns-load-patched [ns ^File file keeps]
  (try    
    (let [content (patch-file (slurp file) (patch-fn ns keeps))]
      (util/ns-load-file content ns file))
    
    ;; check 
    (when (not (find-ns ns))
      (throw (Exception. (format "namespace '%s' not found after loading '%s'" ns (.getPath file)))))
      
    (finally
      ;; drop everything in stash
      (remove-ns 'clj-reload.stash)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj 'clj-reload.stash)))))
