(ns clj-reload.util
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    [clojure.lang LineNumberingPushbackReader]
    [java.io File StringReader]
    [java.net URLClassLoader]))

(def ^:dynamic *log-fn*
  println)

(defn log [& args]
  (when *log-fn*
    (apply *log-fn* args)))

(def reader-opts
  {:read-cond :allow
   ;; under jolt, prefer :jolt branches the same way jolt's own loader does
   :features  #?(:jolt #{:jolt :clj} :clj #{:clj})
   :eof       ::eof})

(def dummy-resolver
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_]
      'user)
    (resolveClass [_ sym]
      sym)
    (resolveAlias [_ sym]
      (symbol (str ":" sym)))
    (resolveVar [_ sym]
      sym)))

(def ^:dynamic *read-ns*
  "Namespace bound as `*ns*` while reading forms. Jolt's reader ignores
  `*reader-resolver*` and resolves `::alias/kw` through the aliases of `*ns*`,
  so `with-read-ns` sets up a scratch namespace with the aliases a file uses."
  nil)

(def ^:private alias-ns-prefix
  "clj-reload.alias.")

(defn- scan-aliases
  "Returns the set of alias symbols used as `::alias/name` in `content`. May
  over-match inside strings and comments, which is harmless."
  [^String content]
  (into #{}
    (map (comp symbol second))
    (re-seq #"::([^\s/\[\]{}()\"';@^`~\\,:]+)/" content)))

(defn call-with-read-ns
  "Calls `(f)` with `*read-ns*` bound to a scratch namespace aliasing every
  `::alias/` used in `content`, then removes the scratch namespaces."
  [^String content f]
  (let [read-ns (create-ns (gensym "clj-reload.read-ns-"))
        targets (for [a (scan-aliases content)]
                  (let [target (create-ns (symbol (str alias-ns-prefix a)))]
                    (binding [*ns* read-ns]
                      (alias a (ns-name target)))
                    (ns-name target)))
        targets (doall targets)]
    (try
      (binding [*read-ns* read-ns]
        (f))
      (finally
        (remove-ns (ns-name read-ns))
        (doseq [t targets]
          (remove-ns t))))))

(defmacro with-read-ns [content & body]
  `(call-with-read-ns ~content (fn [] ~@body)))

(defn read-form [reader]
  (binding [*read-eval*       false
            *suppress-read*   true
            *reader-resolver* dummy-resolver
            *ns*              (or *read-ns* *ns*)]
    (read reader-opts reader)))

(defn throwable? [o]
  (instance? Throwable o))

(defn update! [m k f & args]
  (assoc! m k (apply f (m k) args)))

(def conjs
  (fnil conj #{}))

(def intos
  (fnil into #{}))

(defn assoc-some [m & kvs]
  (reduce
    (fn [m [k v]]
      (cond-> m
        (some? v) (assoc k v)))
    m
    (partition 2 kvs)))

(defn some-map [& kvs]
  (apply assoc-some nil kvs))

(defn some-set [& vals]
  (not-empty
    (set (filter some? vals))))

(defn map-vals [m f]
  (when (some? m)
    (persistent!
      (reduce-kv
        #(assoc! %1 %2 (f %3))
        (transient (empty m))
        m))))

(defn deep-merge [& ms]
  (apply merge-with merge ms))

(defmacro for-map [& body]
  `(into {}
     (for ~@body)))

(defmacro for-set [& body]
  `(into #{}
     (for ~@body)))

(defn doeach [f xs]
  (doseq [x xs]
    (f x)))

(defn now []
  (System/currentTimeMillis))

(defn last-modified [^File f]
  (some-> f .lastModified))

(defn set-last-modified [^File f t]
  (some-> f (.setLastModified t)))

(defn file? [^File f]
  (some-> f .isFile))

(defn directory? [^File f]
  (some-> f .isDirectory))

(defn file-name [^File f]
  (some-> f .getName))

(defn file-path [^File f]
  (some-> f .getPath))

(defn file-delete [^File f]
  (some-> f .delete))

(defn file-reader ^LineNumberingPushbackReader [f]
  (LineNumberingPushbackReader.
    (io/reader (io/file f))))

(defn string-reader ^LineNumberingPushbackReader [^String s]
  (LineNumberingPushbackReader.
    (StringReader. s)))

(defn ns-load-file [content ns ^File file]
  (let [[_ ext] (re-matches #".*\.([^.]+)" (.getName file))
        path    (-> ns str (str/replace #"\-" "_") (str/replace #"\." "/") (str "." ext))]
    #?(:jolt
       ;; jolt has no Compiler/load; load-reader is the closest equivalent
       (binding [*ns*          *ns*
                 *file*        path
                 *source-path* (.getName file)]
         (load-reader (StringReader. content)))
       :clj
       (Compiler/load (StringReader. content) path (.getName file)))))

(defn loader-classpath []
  (->> (clojure.lang.RT/baseLoader)
    (iterate #(.getParent ^ClassLoader %))
    (take-while identity)
    (filter #(instance? URLClassLoader %))
    (mapcat #(.getURLs ^URLClassLoader %))
    (map io/as-file)
    (filter directory?)))

(defn system-classpath []
  (-> (System/getProperty "java.class.path")
    (str/split (re-pattern (System/getProperty "path.separator")))
    (->> (map io/as-file)
      (filter directory?))))

(defn classpath-dirs []
  (->> (or
         (not-empty (loader-classpath))
         (not-empty (system-classpath)))
    (distinct)
    (mapv file-path)))

(comment
  (classpath-dirs)
  (system-classpath))