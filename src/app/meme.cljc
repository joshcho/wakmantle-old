(ns app.meme

  #?(:clj (:import
           [java.util Collections ArrayList Random Collection]))
  (:require
   contrib.str
   #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom :refer [div input props on text style a button span]]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg :refer [svg circle g rect polygon line]]
   ;; [goog.string :as gstring]
   [missionary.core]
   [app.utils :as u :refer [cond-let cmt enter arrows mb-> mb->> mb->>> dropdown]]
   [app.tailwind :refer [tw -tw]]
   #?(:clj
      [app.data :refer [우왁굳 아이네 징버거 비챤 릴파 주르르 고세구 gomem1 gomem2]])
   [app.reactive-render :as rr]
   [app.printer :as p]
   [clojure.core.match :refer [match]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.core.matrix :as m]
   ;; [malli.core :as m]
   #?@
   (:cljs
    [[app.icons :as icons]
     ["remarkable" :as remarkable]
     ["remarkable-katex" :as remarkable-katex]
     [goog.dom :as gdom]
     ;; [goog.dom.element :as gdom.element]
     ]
    :clj
    [[wkok.openai-clojure.api :as api]
     ;; [reduce-fsm :as fsm]
     [clojure.core.async :as a]
     ])))

#?(:clj (defonce !conn (d/create-conn {}))) ; database on server
(e/def db) ; injected database ref; Electric defs are always dynamic

;; move these to clj later


#?(:clj
   (do
     (defn embed [s]
       (->> (api/create-embedding
             {:model "text-embedding-ada-002"
              :input [s]})
            :data
            first
            :embedding))
     (defn desc-embeds [descs]
       (->> (api/create-embedding
             {:model "text-embedding-ada-002"
              :input descs})
            :data
            (sort-by :index)
            (map :embedding)
            (map vector descs)
            (into {})))
     (defonce desc-embeds-memo (memoize desc-embeds))
     (defn to-meme-map [v]
       (let [memes       (map first v)
             descs       (map second v)
             desc-embeds (desc-embeds-memo descs)]
         (u/map-vals
          (fn [desc]
            {:desc  desc
             :embed (get desc-embeds desc)})
          (into {} v))))))
;; (convert (map #(str/split % #":" 2) (str/split 비챤 #"\n+")))


#_(declare custom-wakgood)

#?(:clj
   (def wv (u/map-vals
            to-meme-map
            (merge
             {"비챤"
              (map #(str/split % (re-pattern ": ") 2)
                   (str/split 비챤 (re-pattern "\n+")))
              "우왁굳"
              (->> 우왁굳
                   (map (fn [meme-list-str]
                          (filter
                           #(str/includes? % " - ")
                           (str/split meme-list-str (re-pattern "\n")))))
                   (map (fn [meme-list]
                          (map #(str/split % (re-pattern " - ") 2)
                               meme-list)))
                   (apply concat)
                   (into {}))
              "아이네"
              (map #(str/split % (re-pattern ": ") 2)
                   (str/split 아이네 (re-pattern "\n+")))
              "징버거"
              (->>
               (str/split 징버거 (re-pattern "\n+"))
               (map #(str/split % (re-pattern ": ") 2))
               (filter
                (fn [x]
                  (= (count x) 2)))
               (map (fn [[x y]]
                      [(str/trim x) y])))
              "릴파"
              (->>
               (str/split 릴파 (re-pattern "\n+"))
               (map #(str/split % (re-pattern ": ") 2))
               (filter
                (fn [x]
                  (= (count x) 2)))
               (map (fn [[x y]]
                      [(str/trim x) y])))
              "주르르"
              (->>
               (str/split 주르르 (re-pattern "\n+"))
               (map #(str/split % (re-pattern ": ") 2))
               (filter
                (fn [x]
                  (= (count x) 2)))
               (map (fn [[x y]]
                      [(str/trim x) y])))
              "고세구"
              (->>
               (str/split 고세구 (re-pattern "\n+"))
               (map #(str/split % (re-pattern ": ") 2))
               (filter
                (fn [x]
                  (= (count x) 2)))
               (map (fn [[x y]]
                      [(str/trim x) y])))}
             (u/map-vals
              (fn [[s1 s2]]
                (->>
                 (str/split (str s1 "\n" s2)
                            (re-pattern "\n+"))
                 (map #(str/split % (re-pattern ": ") 2))
                 (filter
                  (fn [x]
                    (= (count x) 2)))
                 (map (fn [[x y]]
                        [(str/trim x) y]))))
              gomem1)
             (u/map-vals
              (fn [[s1 s2]]
                (->>
                 (str/split (str s1 "\n" s2)
                            (re-pattern "\n+"))
                 (map #(str/split % (re-pattern ": ") 2))
                 (filter
                  (fn [x]
                    (= (count x) 2)))
                 (map (fn [[x y]]
                        [(str/trim x) y]))))
              gomem2)))))

#?(:clj
   (do
     ))

(defn utf<-str [s]
  #?(:clj
     (.getBytes s "UTF-8")
     :cljs
     (let [encoder     (js/TextEncoder.)
           uint8-array (.encode encoder s)]
       (vec (map vec (partition 3 uint8-array))))))

(defn str<-utf [bytes]
  #?(:clj
     (String. bytes "UTF-8")
     :cljs
     (let [decoder (js/TextDecoder. "utf-8")]
       (.decode decoder bytes))))

#?(:cljs
   (do
     (defn hex<-str [s]
       (reduce (fn [acc c]
                 (let [char-code  (.charCodeAt c 0)
                       hex-value  (.toString (js/Number char-code) 16)
                       padded-hex (clojure.string/replace (str hex-value) #"^.$" "0$&")]
                   (conj acc padded-hex)))
               []
               s))))

(defn cosine-similarity [v1 v2]
  (let [magnitude (fn [v] (Math/sqrt (m/dot v v)))
        dot-prod  (m/dot v1 v2)
        mag1      (magnitude v1)
        mag2      (magnitude v2)]
    (/ dot-prod (* mag1 mag2))))

#?(:clj
   (defn char-code [s]
     (int (first s))))

(defn is-kr [c]
  #?(:cljs
     (let [code (.charCodeAt c 0)]
       (or (<= (.charCodeAt "ㄱ" 0) code (.charCodeAt "ㅎ" 0))
           (<= (.charCodeAt "가" 0) code (.charCodeAt "힣" 0))))
     :clj
     (let [code (char-code c)]
       (or (<= (char-code "ㄱ") code (char-code "ㅎ"))
           (<= (char-code "가") code (char-code "힣"))))))

(defn kr-num [s]
  #?(:cljs
     (- (.charCodeAt s 0) (.charCodeAt "가" 0))
     :clj
     (- (char-code s) (char-code "가"))))

(defn kr-list [s]
  (let [jaeum-list ["ㄱ" "ㄲ" "ㄴ" "ㄷ" "ㄸ" "ㄹ" "ㅁ" "ㅂ" "ㅃ" "ㅅ"
                    "ㅆ" "ㅇ" "ㅈ" "ㅉ" "ㅊ" "ㅋ" "ㅌ" "ㅍ" "ㅎ"]
        kr-num-s   (kr-num s)]
    (if (>= kr-num-s 0)
      (let [res (if (not= 0 (mod kr-num-s 28))
                  (conj [s] (str (char (+ (* (int (/ kr-num-s 28)) 28)

                                          #?(:cljs
                                             (.charCodeAt "가" 0)
                                             :clj
                                             (char-code "가"))))))
                  [s])]
        (conj res (jaeum-list (int (/ kr-num-s 588)))))
      [s])))

(defn eq-kr [s d]
  (if (and (is-kr s) (is-kr d))
    (or
     (u/in? (kr-list d) s)
     (u/in? (kr-list s) d))
    (= s d)))

;; (defn match-incrementally [query data]
;;   (let [query-l (count query)
;;         data    (vec data)]
;;     (some (fn [i]
;;             (every? (fn [j]
;;                       (if (= j (- query-l 1))
;;                         (eq-kr (nth query j) (data (+ i j)))
;;                         (= (nth query j) (data (+ i j)))))
;;                     (range 0 query-l)))
;;           (range 0 (inc (- (count data) query-l))))))

(defn eq-kr-pos [s d d-next]
  (let [jaeum-list ["ㄱ" "ㄲ" "ㄴ" "ㄷ" "ㄸ" "ㄹ" "ㅁ" "ㅂ" "ㅃ" "ㅅ"
                    "ㅆ" "ㅇ" "ㅈ" "ㅉ" "ㅊ" "ㅋ" "ㅌ" "ㅍ" "ㅎ"]
        kr-pos     [[0 0] [1 0] [2 1] [2 9] [4 2] [1 12] [2 18]
                    [7 3] [8 5] [1 0] [2 6] [3 7] [4 9] [5 16]
                    [6 17] [7 18] [16 6] [17 7] [1 9] [19 9] [20 10]
                    [21 11] [22 12] [23 14] [24 15] [25 16] [26 17] [27 18]]
        jaeum-pos  (into {} (map vector jaeum-list kr-pos))]
    (when (and (is-kr s) (is-kr d) (is-kr d-next))
      (let [s-pos         (char
                           (int (-
                                 #?(:cljs
                                    (.charCodeAt s 0)
                                    :clj
                                    (char-code s))
                                 ((kr-pos (mod (kr-num s) 28)) 0))))
            s-pos-chosung (jaeum-list ((kr-pos (mod (kr-num s) 28)) 1))]
        (and (= s-pos d) (some #(= % s-pos-chosung) (kr-list d-next)))))))

(defn kr-match-index [query data]
  (when (and (not-empty query)
             (not-empty data)
             (<= (count query) (count data)))
    ((fn f [d]
       (when (not-empty d)
         (if (every?
              identity
              (map eq-kr
                   (map str (drop-last query))
                   (map str d)))
           (let [dropped-d (map str (drop (dec (count query)) d))]
             (when (not-empty dropped-d)
               (if (or
                    (eq-kr (str (last query))
                           (str (first dropped-d)))
                    (when (<= 2 (count dropped-d))
                      (eq-kr-pos (str (last query))
                                 (str (first dropped-d))
                                 (str (second dropped-d)))))
                 (- (count data) (count d))
                 (f (subs d 1)))))
           (f (subs d 1)))))
     data)))

#?(:cljs
   (do
     (def !history (atom []))

     (defn split-include-delimiter [s delimiters]
       (let [i       (atom 0)
             indices (atom [])]
         (while (< @i (count s))

           (if-let [delimiter
                    (->> delimiters
                         (filter #(str/starts-with? (subs s @i) %))
                         first)]
             (do
               (swap! indices conj [@i (+ @i (count delimiter))])
               (swap! i (partial + (count delimiter))))
             (swap! i inc)))
         (let [last-end (atom 0)]
           (conj
            (apply u/concatv
                   (for [[start end] @indices]
                     (u/prog1
                         [(subs s @last-end start) (subs s start end)]
                       (reset! last-end end))))
            (subs s @last-end)))))))

(defn get-wv [topic]
  #?(:clj
     (u/map-vals
      #(assoc % :topic topic)
      (get wv topic))))

(e/def wv-topic (e/server
                 (apply merge
                        (map get-wv (keys wv)))))


#?(:clj
   (defn seeded-shuffle
     [coll seed]
     (let [al  (java.util.ArrayList. coll)
           rng (java.util.Random. seed)]
       (java.util.Collections/shuffle al rng)
       (vec (.toArray al)))))


(e/def words (e/server (concat ["완고한 고집의 힘 페리도트 그린" "뿌애앵" "살상모드" "새 구운다" "메일단" "감다뒤사우르스"]
                               (seeded-shuffle (keys wv-topic) 42)
                               ;; (shuffle (keys wv-topic))
                               )))
(e/def hard-words (e/server (concat ["징쁘아까오" "알잘딱깔센" "홋치" "햄이네 박사님" "모시깽이"]
                                    ;; (shuffle (keys wv-topic))
                                    (seeded-shuffle (keys wv-topic) 24))))
(e/def today-n (e/server (- (int (quot (+ e/system-time-secs
                                          (* 9 60 60))
                                       (* 24 60 60)))
                            19608)))

(e/defn Main []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (div
        (let [!hard-mode    (atom false)
              hard-mode     (e/watch !hard-mode)
              !value        (atom "")
              value         (e/watch !value)
              !won          (atom false)
              won           (e/watch !won)
              !res          (atom nil)
              res           (e/watch !res)
              history       (e/watch !history)
              !keywords     (atom #{})
              keywords      (e/watch !keywords)
              !best         (atom nil)
              best          (e/watch !best)
              !gave-up      (atom false)
              gave-up       (e/watch !gave-up)
              !view-related (atom false)
              view-related  (e/watch !view-related)
              res-embed     (e/server (when res
                                        (embed res)))
              memes         (when (and wv-topic (not-empty value))
                              (e/server
                               (some->>
                                wv-topic
                                (map
                                 (comp #(vector % (kr-match-index value %)) first))
                                (filter second)
                                (sort-by second)
                                (map first)
                                (take 10))))
              !game-n       (atom today-n)
              game-n        (e/watch !game-n)
              answer        (if hard-mode
                              (nth hard-words game-n)
                              (nth words game-n))
              meme-map
              (e/server
               (let [meme-map wv-topic
                     meme-map (into
                               {}
                               (map (fn [[meme {:keys [embed] :as m}]]
                                      [meme (assoc m :similarity
                                                   (cosine-similarity (get-in meme-map [answer :embed])
                                                                      embed))])
                                    wv-topic))
                     similarities
                     (map
                      :similarity
                      (vals (dissoc meme-map answer)))
                     max      (let [max (apply max similarities)]
                                (+ max (/ (- 1 max) 20)))
                     min      (apply min similarities)
                     diff     (- max min)
                     meme-map (u/map-vals
                               (fn [m]
                                 (update m :similarity
                                         (fn [sim]
                                           2
                                           (* 100
                                              (/ (- sim min)
                                                 diff)))))
                               meme-map)]
                 (into {} (map (fn [[[meme m] i]]
                                 [meme (assoc m :rank i)])
                               (map vector
                                    (reverse (sort-by (comp :similarity second)
                                                      meme-map))
                                    (map inc (range)))))))]
          (when ((set history) answer)
            (reset! !won true))
          #_(div (text
                  (count meme-map)))
          (div
            (tw "mx-auto mt-14 sm:mt-12 max-w-md")
            (div
              (tw "mx-auto w-fit my-2 text-lg font-bold")
              (text "왁맨틀 - 왁타버스 밈 유사도 추측 게임"))
            #_(div (text answer))
            (div
              (tw "mx-4 my-2 border-[black 2] p-2"
                  (when hard-mode
                    "border-red-800"))
              (div
                (let [!temp (atom nil)
                      temp  (e/watch !temp)]
                  (div (tw "flex")
                    (u/input*
                     (or temp value)
                     (e/fn [v]
                       (reset! !temp nil)
                       (reset! !value v))
                     (props {:placeholder "맞춰보세요"})
                     (tw "input-[* sm] rounded-md pl-2 mb-1")
                     (arrows !temp (reverse memes) true)
                     (enter
                      (e/fn [v]
                        (reset! !temp nil)
                        (reset! !value "")
                        (reset! !res v)
                        (if ((set memes) v)
                          (if ((set history) v)
                            (swap! !history
                                   (fn [history]
                                     (conj (vec (remove #(= % v) history)) v)))
                            (do
                              (swap! !best #(if (or (nil? %)
                                                    (< (get-in meme-map [v :rank]) %))
                                              (get-in meme-map [v :rank])
                                              %))
                              (swap! !history conj v)))
                          (swap! !keywords conj v))))))
                  (when (and (not-empty value)
                             (not (empty? memes)))
                    (div
                      (tw "absolute menu bg-base-200 rounded-box absolute z-50 mt-1"
                          "border-[neutral-content 2]")
                      (dropdown
                       (if (empty? memes) ["검색 결과 없음"] memes)
                       (e/fn [v]
                         (when (not (empty? memes))
                           (reset! !temp nil)
                           (reset! !value "")
                           (reset! !res v)
                           (if ((set memes) v)
                             (if ((set history) v)
                               (swap! !history
                                      (fn [history]
                                        (conj (vec (remove #(= % v) history))
                                              v)))
                               (do
                                 (swap! !best #(if (or (nil? %)
                                                       (< % (get-in meme-map [v :rank])))
                                                 (get-in meme-map [v :rank])
                                                 %))
                                 (swap! !history conj v)))
                             (swap! !keywords conj v))))
                       (tw "rounded-sm p-1"
                           (when (= (.-textContent dom/node) temp)
                             "bg-neutral-content")
                           (when (empty? memes)
                             (tw "hover:bg-base-200"
                                 "pointer-events-none")))))))
                (div
                  (tw "text-xs text-gray-600")
                  (text "* 정답에 가까워질수록 흐릿한 설명이 공개됩니다."))))
            (when (or won gave-up)
              (div
                (tw "ml-4 font-bold mb-2 flex")
                (cond won
                      (div
                        (tw "text-green-500")
                        (if hard-mode
                          (text "레게노... 어떻게 한거죠?")
                          (text "킹아! 정답입니다!")))
                      gave-up
                      (div
                        (tw "text-red-500")
                        (text "아쉽탈")))
                (ui/button
                  (e/fn []
                    (swap! !view-related not))
                  (tw "btn-[* xs] rounded-sm font-normal border-[black 1] hover:border-[black 1] ml-2")
                  (text
                   (if view-related
                     "관련 밈 끄기"
                     "관련 밈 보기")))))
            (let [desc       (get-in meme-map [answer :desc])
                  desc-words (when desc
                               (str/split desc #" "))
                  reveal-num (when best
                               (if hard-mode
                                 (cond (> best 256) 0
                                       (> best 196) 1
                                       (> best 148) 2
                                       (> best 80)  3
                                       (> best 56)  4
                                       (> best 32)  5
                                       (> best 16)  7
                                       (> best 8)   9
                                       true         11)
                                 (cond (> best 256) 0
                                       (> best 184) 2
                                       (> best 128) 3
                                       (> best 64)  4
                                       (> best 48)  6
                                       (> best 32)  8
                                       (> best 16)  10
                                       (> best 8)   12
                                       true         14)))
                  reveals    (when (and reveal-num desc-words)
                               (->> (shuffle (range (count desc-words)))
                                    (map (fn [i] (nth desc-words i)))
                                    distinct
                                    (remove keywords)
                                    (take reveal-num)))]
              (div
                (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm mb-2 text-sm"
                    (when hard-mode
                      "border-red-800"))
                (div
                  (tw "flex")
                  (div
                    (tw "w-fit h-5 border-[*] mb-1")
                    (if (or won gave-up)
                      (div
                        (tw "font-bold mr-1 text-green-500")
                        (text answer))
                      (e/for [i (range (count answer))]
                        (let [s
                              (let [s (str (nth answer i))]
                                (if (= s " ")
                                  s
                                  "_"))]
                          (span
                           (tw "mr-1")
                           (text s))))))
                  (div (tw "grow"))
                  (when (or (and best (if hard-mode
                                        (< best 32)
                                        (< best 64))) won gave-up)
                    (div (text (str "[" (get-in meme-map [answer :topic])
                                    "]")))))
                (when view-related
                  (div
                    (tw "mb-2")
                    (e/for [[s score]
                            (->> (keys meme-map)
                                 (map #(vector % (get-in meme-map [% :similarity])))
                                 (sort-by second)
                                 reverse
                                 rest
                                 (take 12))]
                      (div
                        (tw "flex gap-2")
                        (div
                          (tw "cursor-pointer")
                          (when (dom/Hovered?.)
                            (div
                              (tw "absolute bg-neutral-content ml-12 mt-6 rounded-md p-2 text-sm mr-6 sm:mr-0"
                                  "max-w-sm drop-shadow-lg whitespace-pre-line")
                              (text
                               (str
                                (get-in meme-map [s :desc])
                                (when (not= (get-in meme-map [s :topic])
                                            "우왁굳")
                                  (str
                                   "\n["
                                   (get-in meme-map [s :topic])
                                   "]"))))))
                          (text s))
                        (div
                          (tw "grow"))
                        (div
                          (text (subs (str score) 0 5)))))))
                (if (or won gave-up)
                  (div
                    (tw "font-bold mr-1")
                    (text desc))
                  (do
                    (let [words (split-include-delimiter desc (concat reveals keywords))]
                      (e/for [[s i] (map vector
                                         words
                                         (range))]
                        (if (even? i)
                          (e/for [word (str/split s #" ")]
                            (span
                             (tw "text-transparent bg-gray-300 mr-1")
                             (style {"filter" "blur(2px)"})
                             (text
                              (apply str (repeat (count word) "---")))))
                          (span
                           (tw "font-bold mr-1"
                               (when (= i 1) "ml-[-4px]"))
                           (text s)))))))
                #_(div
                    (tw "text-gray-400")
                    (div
                      (span
                       (text "현재 키워드: ["))
                      (e/for [[keyword i] (map vector keywords (range))]
                        (when-not (= i 0)
                          (span
                           (text ",")))
                        (span
                         (text keyword)))
                      (span (text "]"))))))
            (let [{:keys [desc embed]} (get meme-map res)]
              #_(e/for [[s score]
                        (->> (keys meme-map)
                             (map #(vector % (cosine-similarity (get-in meme-map [answer :embed])
                                                                (get-in meme-map [% :embed]))))
                             (sort-by second)
                             reverse)]
                  (div
                    (div (text s))
                    (text score)))
              (when (not-empty history)
                (div
                  (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm text-sm mb-2")
                  (div
                    (tw "flex font-bold mb-2")
                    (div
                      (tw "pl-2 w-8")
                      (text "#"))
                    (div
                      (tw "pl-6")
                      (text "추측한 밈"))
                    (div (tw "grow"))
                    (div
                      (tw "flex w-[153px]")
                      (div
                        (tw "grow")
                        (text "유사도"))
                      (div
                        (tw "pr-2")
                        (text "순위"))))
                  (e/for [[res idx]
                          ((fn push-last-to-top [v]
                             (u/concatv (list (last v))
                                        (->>
                                         (drop-last v)
                                         (sort-by
                                          (fn [[res _]]
                                            (get-in meme-map [res :similarity])))
                                         reverse)))
                           (map vector history
                                (map inc (range))))]
                    (let [first? (= idx (count history))]
                      (div
                        (tw "px-2 py-[1px] flex gap-4"
                            (when first?
                              "border-[b gray-500] mb-2 pb-2"))
                        (div
                          (tw "w-56 flex")
                          (div
                            (tw "w-12")
                            (text idx))
                          (div
                            (tw "hover:cursor-pointer grow")
                            (div
                              ;; don't merge these tw's, super weird bug
                              (if (= res answer)
                                (tw "text-green-500 font-bold")
                                (when first?
                                  (tw "text-red-400 font-bold")))
                              (text res))
                            (when (dom/Hovered?.)
                              (div
                                (tw "absolute bg-neutral-content ml-5 mt-1 rounded-md p-2 text-sm"
                                    "max-w-sm drop-shadow-lg")
                                (text (get-in meme-map [res :desc]))))))
                        (div
                          (tw "flex w-24 sm:w-36")
                          (div
                            (tw "grow")
                            (text
                             (if (= res answer)
                               "100.00"
                               (subs (str (get-in meme-map [res :similarity]))
                                     0 5))))
                          (div
                            (text
                             (str (get-in meme-map [res :rank]))))))))))
              (div (tw "flex")
                (div (tw "grow"))
                (div (tw "text-xs text-gray-500 mt-[-6px] mr-4 mb-[-8px]")
                  (text
                   (let [n (inc game-n)]
                     (cond (= n 1) "첫"
                           (= n 2) "두"
                           (= n 3) "세"
                           (= n 4) "네"
                           (= n 5) "다섯"
                           true    n)) "번째 왁맨틀")))
              (if (or won gave-up)
                (when (< 0 game-n)
                  (ui/button
                    (e/fn []
                      (reset! !hard-mode false)
                      (reset! !won false)
                      (reset! !res nil)
                      (reset! !gave-up false)
                      (reset! !best nil)
                      (reset! !keywords #{})
                      (reset! !view-related false)
                      (reset! !history [])
                      (reset! !game-n (if (< 0 game-n)
                                        (dec game-n)
                                        game-n)))
                    (tw "btn-[* sm] mx-4 mt-1 bg-base-100 hover:bg-base-200 rounded-md font-normal")
                    (text "어제의 왁맨틀")))
                (ui/button
                  (e/fn []
                    (reset! !gave-up true))
                  (tw "btn-[* sm] mx-4 mt-1 bg-base-100 hover:bg-base-200 rounded-md")
                  (text "포기")))))
          (div
            (tw "max-w-lg mx-auto mt-4 text-sm px-4 sm:px-0")
            (div
              (text "Q & A"))
            (e/for [[s1 s2] [["왁맨틀은 무엇인가요?"
                              "왁맨틀은 오늘의 왁타버스 밈을 맞히는 게임입니다. 기존 꼬맨틀 게임과 비슷하지만, 정답에 가까워질수록 부분적으로 설명이 공개됩니다."]
                             ["정답에는 어떤 밈이 포함되어 있나요?"
                              "정답에는 우왁굳, 이세돌, 고멤 관련 밈과 별명 총 579가지가 포함되어 있습니다.\n예) 레게노, 미웡, 선량한 시민"]
                             ["유사도는 무엇인가요?"
                              "왁맨틀에서 추측한 밈과 정답 밈의 설명이 의미맥락적으로 얼마나 비슷한지 정도를 0 에서 100까지의 숫자로 계산한 점수입니다. 숫자가 클수록 유사한 정도가 크다고 이해할 수 있습니다. 유사도 측정을 위해서는 나무위키 설명을 사용하였습니다."]
                             ["너무 쉬워요!"
                              "_"]
                             ["너무 느려요!"
                              "속도 문제는 곧 해결하도록 하겠습니다."]
                             ["소스 코드를 볼 수 있나요?"
                              "킹아입니다."]
                             ["특정 밈이 없어요!"
                              "일단 인물들에 대한 설명은 추가하지 않을 예정입니다. 꼭 중요한 밈이 버그가 있다면 깃헙이나 X로 연락주세요."]]]
              (dom/b
               (tw "mt-2")
               (if (and (= s1 "너무 쉬워요!")
                        hard-mode)
                 (text "너무 어려워요...")
                 (text s1)))
              (if (= s1 "너무 쉬워요!")
                (div
                  (tw "text-xs mb-2 whitespace-pre-line")
                  (if hard-mode
                    (ui/button
                      (e/fn []
                        (reset! !hard-mode false)
                        (reset! !won false)
                        (reset! !res nil)
                        (reset! !gave-up false)
                        (reset! !best nil)
                        (reset! !keywords #{})
                        (reset! !view-related false))
                      (tw "btn-[* xs] text-xs rounded-sm border-[black 1] mx-1 hover:border-[black 1] mt-[4px]")
                      (text "다시 이지모드"))
                    (div
                      (text "그러면")
                      (ui/button
                        (e/fn []
                          (reset! !hard-mode true)
                          (reset! !won false)
                          (reset! !res nil)
                          (reset! !gave-up false)
                          (reset! !best nil)
                          (reset! !keywords #{})
                          (reset! !view-related false))
                        (tw "btn-[* xs] rounded-sm border-[black 1] mx-1 hover:border-[black 1] hover:bg-gray-900 hover:text-white mt-[4px]")
                        (text "하드모드"))
                      (text "한번 도전해보세요."))))
                (div
                  (tw "sm:text-xs mb-2 whitespace-pre-line")
                  (if (= s2 "킹아입니다.")
                    (a
                     (props {:href   "https://github.com/joshcho/wakmantle"
                             :target "_blank"})
                     (tw "text-blue-600 hover:text-blue-800 cursor-pointer")
                     (text s2))
                    (text s2)))))))
        (div
          (tw "flex gap-4 w-fit mx-auto my-4")
          (e/for [[src site] [["img/github.png" "https://github.com/joshcho"] ["img/twitter.png" "https://twitter.com/eating_entropy"]]]
            (a
             (props {:href   site
                     :target "_blank"})
             (dom/img
              (tw "rounded-lg opacity-30 hover:opacity-70 hover:cursor-pointer")
              (props {:src    src
                      :height "30px"
                      :width  "30px"}))))))))))

#_(def custom-wakgood
    {"고멤"
     "우왁굳의 시청자들 중에서 같이 방송을 할 멤버를 뽑는 콘텐츠이다. 우왁굳의 콘텐츠에 시청자 참여를 하면서 우왁굳과 같이 놀 수 있는 시청자로서의 최고 권한이 주어지는 것이다. 주로 VRChat 기반이다."
     })
