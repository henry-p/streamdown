(ns ansi.hiccup.stream
  "ANSI escape sequence parser that streams into a continuously well-formed Hiccup tree"
  (:require [clojure.string :as str]))

(def esc (char 27))
(def bel (char 7))

(def ^:private sgr-color-base
  {30 :black, 31 :red, 32 :green, 33 :yellow, 34 :blue, 35 :magenta, 36 :cyan, 37 :white})

(def ^:private sgr-bright-color-base
  {90 :black, 91 :red, 92 :green, 93 :yellow, 94 :blue, 95 :magenta, 96 :cyan, 97 :white})

(def ^:private bg-color-base
  {40 :black, 41 :red, 42 :green, 43 :yellow, 44 :blue, 45 :magenta, 46 :cyan, 47 :white})

(def ^:private bg-bright-color-base
  {100 :black, 101 :red, 102 :green, 103 :yellow, 104 :blue, 105 :magenta, 106 :cyan, 107 :white})

(defn- empty-attrs []
  {:bold? false, :italic? false, :underline? false
   :fg nil, :fg-bright? false
   :bg nil, :bg-bright? false
   :fg-rgb nil, :bg-rgb nil
   :fg-256 nil, :bg-256 nil
   :href nil})

(defn new-state
  ([] (new-state {}))
  ([{:keys [root-tag]
     :or   {root-tag [:pre.terminal]}}]
   {:root-tag root-tag
    :pending ""
    :attrs (empty-attrs)
    :segments []}))

(defn- sb-append! ^StringBuilder [^StringBuilder sb ^CharSequence s]
  (doto sb (.append (str s))))

(defn- segment
  ([attrs] {:attrs attrs, :sb (StringBuilder.)})
  ([attrs s]
   {:attrs attrs
    :sb (doto (StringBuilder.) (.append (str s)))}))

(defn- same-visual-attrs? [a b]
  (and (= (dissoc a :href) (dissoc b :href))
       (= (:href a) (:href b))))

(defn- push-or-append [state s]
  (let [curr (:attrs state)
        segs (:segments state)
        last-seg (peek segs)]
    (if (and last-seg (same-visual-attrs? (:attrs last-seg) curr))
      (do (sb-append! (:sb last-seg) s)
          (assoc state :segments (conj (pop segs) last-seg)))
      (assoc state :segments (conj segs (segment curr s))))))

(defn- apply-sgr-codes [attrs codes]
  (loop [a attrs, cs codes]
    (if (empty? cs)
      a
      (let [c (first cs)
            more (rest cs)]
        (cond
          (or (nil? c) (= c 0))
          (recur (-> (empty-attrs) (assoc :href (:href attrs))) more)

          (= c 1) (recur (assoc a :bold? true) more)
          (= c 3) (recur (assoc a :italic? true) more)
          (= c 4) (recur (assoc a :underline? true) more)
          (= c 22) (recur (assoc a :bold? false) more)
          (= c 23) (recur (assoc a :italic? false) more)
          (= c 24) (recur (assoc a :underline? false) more)

          (contains? sgr-color-base c)
          (recur (-> a (assoc :fg (get sgr-color-base c)
                              :fg-bright? false
                              :fg-rgb nil :fg-256 nil)) more)

          (contains? sgr-bright-color-base c)
          (recur (-> a (assoc :fg (get sgr-bright-color-base c)
                              :fg-bright? true
                              :fg-rgb nil :fg-256 nil)) more)

          (contains? bg-color-base c)
          (recur (-> a (assoc :bg (get bg-color-base c)
                              :bg-bright? false
                              :bg-rgb nil :bg-256 nil)) more)

          (contains? bg-bright-color-base c)
          (recur (-> a (assoc :bg (get bg-bright-color-base c)
                              :bg-bright? true
                              :bg-rgb nil :bg-256 nil)) more)

          (= c 39) (recur (-> a (assoc :fg nil :fg-bright? false :fg-rgb nil :fg-256 nil)) more)
          (= c 49) (recur (-> a (assoc :bg nil :bg-bright? false :bg-rgb nil :bg-256 nil)) more)

          (= c 38)
          (let [mode (first more)]
            (cond
              (= mode 5)
              (let [n (second more)]
                (if (>= (count more) 2)
                  (recur (-> a (assoc :fg nil :fg-256 n :fg-rgb nil)) (drop 2 more))
                  (recur a more)))

              (= mode 2)
              (let [rgb-values (take 3 (rest more))
                    [r g b] rgb-values]
                (if (= 3 (count rgb-values))
                  (recur (-> a (assoc :fg-rgb [r g b] :fg nil :fg-256 nil)) (drop 4 more))
                  (recur a more)))

              :else (recur a more)))

          (= c 48)
          (let [mode (first more)]
            (cond
              (= mode 5)
              (let [n (second more)]
                (if (>= (count more) 2)
                  (recur (-> a (assoc :bg nil :bg-256 n :bg-rgb nil)) (drop 2 more))
                  (recur a more)))

              (= mode 2)
              (let [rgb-values (take 3 (rest more))
                    [r g b] rgb-values]
                (if (= 3 (count rgb-values))
                  (recur (-> a (assoc :bg-rgb [r g b] :bg nil :bg-256 nil)) (drop 4 more))
                  (recur a more)))

              :else (recur a more)))

          :else (recur a more))))))

(defn- start-link [attrs href]
  (assoc attrs :href (when (seq (str/trim (or href ""))) href)))

(defn- end-link [attrs]
  (assoc attrs :href nil))

(defn- csi-final? [ch]
  (let [cp (int ch)] (<= 0x40 cp 0x7E)))

(defn- parse-int-safe [s]
  (try (Integer/parseInt s)
       (catch Throwable _ nil)))

(defn- parse-csi-sgr [s]
  (let [s (or s "")]
    (if (str/blank? s)
      [0]
      (->> (str/split s #";")
           (map parse-int-safe)
           (remove nil?)
           (vec)
           (#(if (seq %) % [0]))))))

(defn- parse-osc [payload]
  (when (seq payload)
    (let [[code-str & more] (str/split payload #";" 2)
          code (parse-int-safe code-str)
          rest (first more)]
      (when code {:code code :rest rest}))))

(defn- chop-prefix [s n]
  (if (>= n (count s))
    ""
    (subs s n)))

(defn- parse-step [pending]
  (let [i (str/index-of pending (str esc))]
    (cond
      (or (nil? i) (neg? i))
      (when (pos? (count pending))
        [{:type :text :s pending} ""])

      (> i 0)
      [{:type :text :s (subs pending 0 i)} (subs pending i)]

      :else
      (if (< (count pending) 2)
        [nil pending]
        (let [c1 (.charAt pending 1)]
          (case c1
            \[
            (let [rest (subs pending 2)
                  idx (loop [j 0]
                        (when (< j (count rest))
                          (let [ch (.charAt rest j)]
                            (if (csi-final? ch)
                              j
                              (recur (inc j))))))]
              (if (nil? idx)
                [nil pending]
                (let [final (.charAt rest idx)
                      params (subs rest 0 idx)
                      consumed (+ 2 1 idx)]
                  (if (= final \m)
                    [{:type :sgr :codes (parse-csi-sgr params)} (chop-prefix pending consumed)]
                    [{:type :text :s ""} (chop-prefix pending consumed)]))))

            \]
            (let [rest (subs pending 2)
                  bel-idx (str/index-of rest (str bel))
                  st-idx (str/index-of rest (str esc \\))
                  term-idx (cond
                             (and bel-idx st-idx) (min bel-idx st-idx)
                             bel-idx bel-idx
                             st-idx st-idx
                             :else nil)]
              (if (nil? term-idx)
                [nil pending]
                (let [payload (subs rest 0 term-idx)
                      consumed (+ 2 term-idx (if (= term-idx bel-idx) 1 2))
                      parsed (parse-osc payload)]
                  (if (and parsed (= 8 (:code parsed)))
                    (let [val (:rest parsed)
                          [_ params uri] (str/split (or val "") #";" 3)
                          uri (or uri params "")
                          phase (if (seq (str/trim uri)) :start :end)]
                      [{:type :osc-link :phase phase :href (when (= phase :start) uri)}
                       (chop-prefix pending consumed)])
                    [{:type :text :s ""} (chop-prefix pending consumed)]))))

            [{:type :text :s ""} (subs pending 1)]))))))

(defn- drain-tokens [pending]
  (loop [p pending, acc []]
    (let [[tok p2] (parse-step p)]
      (cond
        (nil? tok) [acc p]
        :else (recur p2 (if (and (= (:type tok) :text) (empty? (:s tok))) acc (conj acc tok)))))))

(defn- apply-token [state tok]
  (case (:type tok)
    :text (push-or-append state (:s tok))
    :sgr  (assoc state :attrs (apply-sgr-codes (:attrs state) (:codes tok)))
    :osc-link (assoc state :attrs (case (:phase tok)
                                    :start (start-link (:attrs state) (:href tok))
                                    :end   (end-link (:attrs state))))
    state))

(defn ingest [state chunk]
  (let [pending (str (:pending state) (str chunk))
        [toks rest] (drain-tokens pending)]
    (-> (reduce apply-token (assoc state :pending rest) toks))))

(defn- classes-for-attrs
  [{:keys [bold? italic? underline? fg bg fg-bright? bg-bright? fg-256 bg-256]}]
  (->> [(when bold? "b")
        (when italic? "i")
        (when underline? "u")
        (when fg (str "fg-" (when fg-bright? "bright-") (name fg)))
        (when bg (str "bg-" (when bg-bright? "bright-") (name bg)))
        (when fg-256 (str "fg-256-" fg-256))
        (when bg-256 (str "bg-256-" bg-256))]
       (remove nil?)
       (str/join " ")
       (not-empty)))

(defn- style-for-attrs
  [{:keys [fg-rgb bg-rgb]}]
  (let [s (cond-> []
            fg-rgb (conj (format "color: rgb(%d,%d,%d)" (nth fg-rgb 0) (nth fg-rgb 1) (nth fg-rgb 2)))
            bg-rgb (conj (format "background-color: rgb(%d,%d,%d)" (nth bg-rgb 0) (nth bg-rgb 1) (nth bg-rgb 2))))]
    (when (seq s) (str/join ";" s))))

(defn- attrs->hiccup-attrs [attrs]
  (let [cls (classes-for-attrs attrs)
        sty (style-for-attrs attrs)]
    (cond-> {}
      cls (assoc :class cls)
      sty (assoc :style sty))))

(defn- seg->hiccup [{:keys [^StringBuilder sb attrs]}]
  (let [s (.toString sb)
        href (:href attrs)
        span-attrs (attrs->hiccup-attrs (dissoc attrs :href))
        node (if (seq span-attrs)
               [:span span-attrs s]
               s)]
    (if href
      [:a {:href href :class "t-link"} node]
      node)))

(defn state->hiccup [{:keys [root-tag segments]}]
  (into (vec root-tag)
        (map seg->hiccup segments)))

(defn reset-styles [state]
  (assoc state :attrs (assoc (empty-attrs) :href (-> state :attrs :href))))

(defn clear [state]
  (assoc state :segments [] :pending ""))

(def demo
  (str "Booting " esc "[1mservice" esc "[0m on " esc "[34mport 3000" esc "[0m\n"
       esc "]8;;https://example.com" bel "Open docs" esc "]8;;" bel " now\n"
       esc "[3mitalic" esc "[23m normal " esc "[4munderline" esc "[24m\n"
       esc "[38;2;200;50;50mTruecolor FG" esc "[0m and " esc "[48;2;10;10;80mBG" esc "[0m\n"))
