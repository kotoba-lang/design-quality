(ns design-quality.cli
  "CLI for design-quality.audit — CI-gate ready.

     score <file-or-dir>... [--weights <edn-file>] [--skip axis1,axis2]
                            [--min <score>] [--edn|--report]

   Scans the given .html files (directories are walked recursively), scores
   each page plus the aggregate, and prints a human report (default) or EDN
   (--edn). Exits 1 when the aggregate falls below --min.

   Runs on nbb (Node) and babashka (JVM optional):

     nbb -m design-quality.cli score test/fixtures/
     bb  -m design-quality.cli score test/fixtures/*.html
     bb  score test/fixtures/*.html            ;; bb.edn task

   The engine (design-quality.audit) stays pure; all IO lives here behind
   reader conditionals (nbb: node fs, bb/clj: clojure.java.io)."
  (:require [clojure.string :as str]
            [design-quality.audit :as audit]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            #?(:clj [clojure.java.io :as io])
            #?(:cljs ["fs" :as fs])))

;; --- IO (platform-specific, CLI-only) ----------------------------------------

(defn- slurp-file [p]
  #?(:clj (slurp p)
     :cljs (fs/readFileSync p "utf8")))

(defn- directory? [p]
  #?(:clj (.isDirectory (io/file p))
     :cljs (and (fs/existsSync p) (.isDirectory (fs/statSync p)))))

(defn- walk-files [dir]
  (let [dir (str/replace dir #"/+$" "")]   ;; normalize trailing slash (same paths on JVM and node)
    #?(:clj (->> (file-seq (io/file dir))
                 (filter #(.isFile ^java.io.File %))
                 (map #(.getPath ^java.io.File %)))
       :cljs (mapcat (fn [ent]
                       (let [p (str dir "/" (.-name ent))]
                         (if (.isDirectory ent) (walk-files p) [p])))
                     (fs/readdirSync dir #js {:withFileTypes true})))))

(defn- exit! [code]
  #?(:clj (System/exit code)
     :cljs (js/process.exit code)))

;; --- deterministic number formatting (identical output on bb and nbb) ---------

(defn fmt2
  "Fixed 2-decimal string, identical across JVM and JS (no `format`, which
   nbb lacks and whose float rendering differs between hosts)."
  [x]
  (let [n (Math/round (* 100.0 (double x)))
        neg? (neg? n)
        n (long (if neg? (- n) n))
        i (quot n 100)
        f (rem n 100)]
    (str (when neg? "-") i "." (when (< f 10) "0") f)))

;; --- argument parsing ----------------------------------------------------------

(defn parse-args
  "[args] → {:paths [...] :weights-file s :skip #{kw} :min n :format :report|:edn}
   or {:error msg}."
  [args]
  (loop [args args acc {:paths [] :format :report}]
    (if-let [a (first args)]
      (case a
        "--weights" (if-let [v (second args)]
                      (recur (nnext args) (assoc acc :weights-file v))
                      {:error "--weights needs an EDN file argument"})
        "--skip"    (if-let [v (second args)]
                      (recur (nnext args)
                             (assoc acc :skip (into #{} (map keyword) (str/split v #","))))
                      {:error "--skip needs a comma-separated axis list"})
        "--min"     (if-let [v (second args)]
                      (if-let [n (parse-double (second args))]
                        (recur (nnext args) (assoc acc :min n))
                        {:error (str "--min needs a number, got: " v)})
                      {:error "--min needs a number argument"})
        "--edn"     (recur (next args) (assoc acc :format :edn))
        "--report"  (recur (next args) (assoc acc :format :report))
        (if (str/starts-with? a "--")
          {:error (str "unknown option: " a)}
          (recur (next args) (update acc :paths conj a))))
      acc)))

(defn- collect-pages
  "Resolve file/dir args to a sorted {page-path → source} map. Directories are
   walked recursively for .html files; explicit file args are taken as-is."
  [paths]
  (let [files (->> paths
                   (mapcat (fn [p]
                             (if (directory? p)
                               (filter #(str/ends-with? % ".html") (walk-files p))
                               [p])))
                   distinct
                   sort)]
    (into (sorted-map) (map (fn [f] [f (slurp-file f)])) files)))

;; --- report rendering ------------------------------------------------------------

(defn render-report
  "Human-readable report for an `audit` result → string."
  [{:keys [overall pages findings]} min-score]
  (let [page-lines
        (mapcat (fn [[name {:keys [overall axes]}]]
                  (cons (str "  " (fmt2 overall) "  " name)
                        (for [{:keys [id weight finding]} axes
                              :when finding]
                          (str "         - " (clojure.core/name id)
                               " (w=" weight "): " finding))))
                pages)
        finding-lines
        (if (empty? findings)
          ["  (none — converged)"]
          (for [{:keys [axis pages finding headroom]} findings]
            (str "  " (name axis) "  headroom=" (fmt2 headroom)
                 "  pages=" (str/join "," pages) "\n    " finding)))
        gate-lines
        (when min-score
          [(str "gate: aggregate " (fmt2 overall)
                (if (< overall min-score)
                  (str " < min " (fmt2 min-score) " -> FAIL")
                  (str " >= min " (fmt2 min-score) " -> PASS")))])]
    (str/join "\n"
              (concat [(str "design-quality audit — " (count pages) " page(s)") ""]
                      page-lines
                      ["" (str "aggregate: " (fmt2 overall)) ""
                       "findings (headroom-first):"]
                      finding-lines
                      gate-lines))))

;; --- entry ------------------------------------------------------------------------

(defn run
  "Execute the CLI on argv (without exiting) → {:exit int :result audit-map}.
   Prints the report/EDN to stdout, errors to stdout with usage."
  [args]
  (let [[cmd & rest-args] args]
    (if (not= cmd "score")
      (do (println "usage: score <file-or-dir>... [--weights <edn-file>] [--skip axis1,axis2] [--min <score>] [--edn|--report]")
          {:exit (if (nil? cmd) 0 1)})
      (let [{:keys [error paths weights-file skip min format]} (parse-args rest-args)]
        (cond
          error (do (println (str "error: " error)) {:exit 1})
          (empty? paths) (do (println "error: no files or directories given") {:exit 1})
          :else
          (let [pages (collect-pages paths)
                opts (cond-> {}
                       weights-file (assoc :weights (edn/read-string (slurp-file weights-file)))
                       skip (assoc :skip skip))
                result (audit/audit pages opts)
                gated? (and min (< (:overall result) min))]
            (if (= format :edn)
              (prn result)
              (println (render-report result min)))
            {:exit (if gated? 1 0) :result result}))))))

(defn -main [& args]
  (let [{:keys [exit]} (run args)]
    (when (pos? exit) (exit! exit))))
