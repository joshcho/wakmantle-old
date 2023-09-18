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
   [app.utils :as u :refer [cond-let cmt enter arrows mb-> mb->> mb->>> dropdown
                            >defn >fn letm]]
   [app.central :as central :refer [defstate]]
   [app.tailwind :refer [tw -tw]]
   #?(:clj
      [app.data :refer [우왁굳 아이네 징버거 비챤 릴파 주르르 고세구 gomem1 gomem2] :as data])
   [app.printer :as p]
   [app.kr :as kr]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.core.matrix :as mat]
   [malli.core :as m]
   #?@
   (:cljs
    [
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

(defstate ctr-state
  {global-state  {:ctr/host        :server
                  n-todays-answers 3
                  :_answer-seed    42
                  :_today          (int e/system-time-secs)
                  :_todays-answers ["아자뾰" "염소희" "권타버스"] ; fill out later
                  _hint-count-fn   (fn [best-rank]
                                     (if (< best-rank 200) 5 0))}
   session-state {answer-state {:ctr/host      :client
                                :answer-idx    0
                                _answer        (nth (:todays-answers global-state)
                                                    answer-idx)
                                :_on-last-idx  (= answer-idx (dec n-todays-answers))
                                :_on-first-idx (= answer-idx 0)}
                  play-state   {recent-guess nil
                                past-guesses []}}})



#_(defstate ctr-state
    {game-state {:answer-idx 0
                 :_answers   ["a" "b"]
                 _answer     (nth answers answer-idx)}
     ab         1
     :_name     "hello"})


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
       (let [descs       (map second v)
             desc-embeds (desc-embeds-memo descs)]
         (u/map-vals
          (fn [desc]
            {:desc  desc
             :embed (get desc-embeds desc)})
          (into {} v))))))

;; (convert (map #(str/split % #":" 2) (str/split 비챤 #"\n+")))

#?(:clj
   (do


     (def wv
       (letfn [(handle-colon [streamer-var]
                 (->>
                     (str/split streamer-var (re-pattern "\n+"))
                   (map #(str/split % (re-pattern ": ") 2))
                   (filter
                    (fn [x]
                      (= (count x) 2)))
                   (map (fn [[x y]]
                          [(str/trim x) y]))))
               (handle-gomem [x]
                 (let [[s1 s2] x]
                   (->>
                       (str/split (str s1 "\n" s2)
                                  (re-pattern "\n+"))
                     (map #(str/split % (re-pattern ": ") 2))
                     (filter
                      (fn [x]
                        (= (count x) 2)))
                     (map (fn [[x y]]
                            [(str/trim x) y])))))]
         (u/map-vals
          to-meme-map
          (merge
           {"우왁굳"
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
            "아이네" (handle-colon 아이네)
            "징버거" (handle-colon 징버거)
            "릴파"   (handle-colon 릴파)
            "주르르" (handle-colon 주르르)
            "고세구" (handle-colon 고세구)
            "비챤"   (handle-colon 비챤)}
           (u/map-vals handle-gomem gomem1)
           (u/map-vals handle-gomem gomem2)))))
     (assert (m/validate [:map-of :string
                          [:map-of :string
                           [:map
                            [:desc :string]
                            [:embed [:vector :any]]]]]
                         wv)
             "Something wrong with wv")))

(defn cosine-similarity [v1 v2]
  (let [magnitude (fn [v] (Math/sqrt (mat/dot v v)))
        dot-prod  (mat/dot v1 v2)
        mag1      (magnitude v1)
        mag2      (magnitude v2)]
    (/ dot-prod (* mag1 mag2))))

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

(e/def map->desc&embed (e/server
                        (apply merge
                               (map get-wv (keys wv)))))

#?(:clj
   (defn seeded-shuffle
     [coll seed]
     (let [al  (java.util.ArrayList. coll)
           rng (java.util.Random. seed)]
       (java.util.Collections/shuffle al rng)
       (vec (.toArray al)))))


(e/def words (e/server (concat
                        ;; ["완고한 고집의 힘 페리도트 그린" "뿌애앵" "살상모드" "새 구운다" "메일단" "감다뒤사우르스"]
                        ;; (seeded-shuffle (keys map->desc&embed) 42)
                        (shuffle (keys map->desc&embed)))))
;; (e/def hard-words (e/server (concat ["징쁘아까오" "알잘딱깔센" "홋치" "햄이네 박사님" "모시깽이"]
;;                                     ;; (shuffle (keys map->desc&embed))
;;                                     (seeded-shuffle (keys map->desc&embed) 24))))

(e/defn Q&A []
  (div
    (tw "max-w-lg mx-auto mt-4 text-sm px-4 sm:px-0")
    (div
      (text "Q & A"))
    (e/for [[s1 s2] [["왁맨틀은 무엇인가요?"
                      "왁맨틀은 오늘의 왁타버스 밈을 맞히는 게임입니다. 기존 꼬맨틀 게임과 비슷하지만, 정답에 가까워질수록 부분적으로 설명을 공개할 수 있습니다."]
                     ["정답에는 어떤 밈이 포함되어 있나요?"
                      "정답에는 우왁굳, 이세돌, 고멤 관련 밈과 별명 총 579가지가 포함되어 있습니다.\n예) 레게노, 미웡, 선량한 시민"]
                     ["정답은 어떻게 맞추나요?"
                      "1. 유사성 순위가 높은 밈을 맞춰 누적 힌트를 얻으세요.\n2. 왁타버스 구성원들을 검색해서 누구의 밈인지 알아가세요."]
                     ["점수는 어떻게 집결되나요?"
                      "총 5번 라운드를 하며, 매번 집계는 20점 만점에 추측 당 1점 감점이 됩니다. 누적힌트가 남아있을 경우 점수에 추가 됩니다. 즉 총 100점 만점 입니다."]
                     ["유사도는 무엇인가요?"
                      "왁맨틀에서 추측한 밈과 정답 밈의 설명이 의미맥락적으로 얼마나 비슷한지 정도를 0 에서 100까지의 숫자로 계산한 점수입니다. 숫자가 클수록 유사한 정도가 크다고 이해할 수 있습니다. 유사도 측정을 위해서는 나무위키 설명을 사용하였습니다."]
                     ["너무 느려요!"
                      "속도 문제는 곧 해결하도록 하겠습니다."]
                     ["소스 코드를 볼 수 있나요?"
                      "킹아입니다."]
                     ["특정 밈이 없어요!"
                      "꼭 들어가야할 밈이나 치명적인 버그가 있다면 깃헙이나 X로 연락주세요."]]]
      #_(dom/b
         (tw "mt-2")
         (if (and (= s1 "너무 쉬워요!")
                  hard-mode)
           (text "너무 어려워요...")
           (text s1)))
      (dom/b
       (tw "mt-2")
       (text s1))
      (div
        (tw "sm:text-xs mb-2 whitespace-pre-line")
        (if (= s2 "킹아입니다.")
          (a
           (props {:href   "https://github.com/joshcho/wakmantle"
                   :target "_blank"})
           (tw "text-blue-600 hover:text-blue-800 cursor-pointer")
           (text s2))
          (text s2)))
      #_(if (= s1 "너무 쉬워요!")
          (div
            (tw "text-xs mb-2 whitespace-pre-line")
            (if hard-mode
              (ui/button
                (e/fn []
                  (reset! !hard-mode false)
                  (reset-fn))
                (tw "btn-[* xs] text-xs rounded-sm border-[black 1] mx-1 hover:border-[black 1] mt-[4px]")
                (text "다시 이지모드"))
              (div
                (text "그러면")
                (ui/button
                  (e/fn []
                    (reset! !hard-mode true)
                    (reset-fn))
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

(e/def member->group-name
  (e/server
   (into {"고멤"         "고멤"
          "고정멤버"     "고멤"
          "이세돌"       "이세돌"
          "이세계아이돌" "이세돌"}
     (mapcat (fn [[groups members]]
               (if (= members #{"우왁굳"})
                 [["우왁굳" :himself]]
                 (for [m members]
                   [m (first groups)])))
             data/group-names->members))))

(e/defn HistoryDisplay [game-state meme-map most-recent-guess
                        history member-group-pair-guesses play-flags reset-fn]
  (let [{:keys [game-n !game-n answer]} game-state
        {:keys [desc embed]}            (get meme-map most-recent-guess)
        {:keys [won gave-up !gave-up]}  play-flags]
    (when (or (not-empty member-group-pair-guesses) (not-empty history))
      (div
        (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm text-sm mb-2")
        (div
          (tw "flex font-bold mb-2")
          (div
            (tw "pl-2 w-8")
            (text "#"))
          (div
            (tw "pl-6")
            (text "추측한 밈/인물"))
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
                          "max-w-sm drop-shadow-lg whitespace-pre-line")
                      (text (str (get-in meme-map [res :desc])
                                 "\n["
                                 (get-in meme-map [res :topic])
                                 "]"))))))
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
                   (str (get-in meme-map [res :rank]))))))))
        (e/for [[member group] (reverse member-group-pair-guesses)]
          (div
            (tw "px-2 py-[1px] flex gap-4")
            (div
              (tw "w-56 flex")
              (div
                (tw "w-12")
                (text "-"))
              (div
                (tw "grow")
                (div (text member))))
            (div
              (tw "flex w-24 sm:w-36")
              (div
                (tw "grow")
                (text (cond (= member (get-in meme-map [answer :topic]))
                            "주인"
                            (= (member->group-name member)
                               (member->group-name (get-in meme-map [answer :topic])))
                            "근접"
                            :else
                            "-")))
              (div
                (tw "mr-1")
                (text "-")))))))
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
    (div
      (tw "flex gap-2 mt-1 ml-4")
      (when-not (or won gave-up)
        (ui/button
          (e/fn []
            ;; (reset! !gave-up true)
            )
          (dom/on! "click"
                   (fn [e]
                     (when (js/confirm "포기하면 이번 라운드 점수는 10점이 감점됩니다.")
                       (reset! !gave-up true))))
          (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md")
          (text "포기")))
      (when (< 0 game-n)
        (ui/button
          (e/fn []
            (reset-fn)
            (reset! !game-n (if (< 0 game-n)
                              (dec game-n)
                              game-n)))
          (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md font-normal")
          (text "<<")))
      (ui/button
        (e/fn []
          (reset-fn)
          (swap! !game-n inc))
        (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md font-normal")
        (text ">>")))))


(e/defn Main []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (div
        (div (text ctr-state))
        (div (text answer))
        (ui/button
          (e/fn []
            (swap!
             (:!answer-idx answer-state)
             inc))
          (text "hello"))
        (letm {play-flags        {hard-mode false
                                  won       false
                                  gave-up   false
                                  _finished (or won gave-up)}
               state
               {play-state {_history                  (e/watch !history)
                            member-group-pair-guesses []
                            keywords                  #{}
                            best                      nil
                            used-hint-count           0
                            _earned-hint-count
                            (when best
                              (cond (> best 256) 0
                                    (> best 144) 1
                                    (> best 96)  2
                                    (> best 48)  3
                                    :else        4))
                            _starting-hint-count      3
                            }
                game-state {game-n  (e/snapshot
                                     (e/server (- (int (quot (+ e/system-time-secs
                                                                (* 9 60 60))
                                                             (* 24 60 60)))
                                                  19608)))
                            _answer (nth words game-n)}}
               preferences       {view-related false}
               most-recent-guess nil
               value             ""}
          (p/peek play-flags)
          (p/peek play-state)
          (p/peek keywords)
          (p/peek most-recent-guess)
          (p/peek map->desc&embed)
          (let [memes         (when (and map->desc&embed (not-empty value))
                                (e/server
                                 (some->>
                                  (concat (keys map->desc&embed)
                                          (->> (mapcat (fn [[group-names members]]
                                                         (concat group-names members))
                                                       (e/server data/group-names->members))
                                            (remove nil?)))
                                  (map #(vector % (kr/kr-match-index value %)))
                                  (filter second)
                                  (sort-by second)
                                  (map first)
                                  (take 10))))
                meme-map      (e/server
                               (let [meme-map map->desc&embed
                                     meme-map (into
                                                  {}
                                                (map (fn [[meme {:keys [embed] :as m}]]
                                                       [meme (assoc m :similarity
                                                                    (cosine-similarity (get-in meme-map [answer :embed])
                                                                                       embed))])
                                                     map->desc&embed))
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
                                                    (map inc (range)))))))
                reset-fn      (fn []
                                (reset! !won false)
                                (reset! !most-recent-guess nil)
                                (reset! !gave-up false)
                                (reset! !member-group-pair-guesses [])
                                (reset! !best nil)
                                (reset! !history [])
                                (reset! !keywords #{})
                                (reset! !view-related false)
                                (reset! !used-hint-count 0))
                answer-member (get-in meme-map [answer :topic])
                extra-points  0
                ;; (cond (u/in? (flatten member-group-pair-guesses) answer-member)
                ;;       (if (= answer-member "우왁굳")
                ;;         1 2)
                ;;       (u/in? (flatten member-group-pair-guesses)
                ;;              (member->group-name answer-member))
                ;;       1
                ;;       :else
                ;;       0)
                hint-count    (+ starting-hint-count
                                 (- earned-hint-count used-hint-count)
                                 extra-points)]
            (p/peek extra-points)
            (p/peek hint-count)
            (when ((set history) answer)
              (reset! !won true))
            #_(div (text
                    (count meme-map)))
            (div
              (tw "mx-auto mt-14 sm:mt-12 max-w-md")
              (div
                (tw "mx-auto w-fit my-2 text-lg font-bold")
                (text "오늘의 왁맨틀 - 왁타버스 밈 추측 게임"))
              #_(div (text answer))
              (div
                (tw "mx-4 my-2 border-[black 2] p-2"
                    #_(when hard-mode
                        "border-red-800"))
                (div
                  (letm {temp       nil
                         alert-word nil}
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
                          (reset! !most-recent-guess v)
                          (reset! !alert-word nil)
                          (if (member->group-name v)
                            (do
                              (if ((set (map first member-group-pair-guesses))
                                   v)
                                (swap! !member-group-pair-guesses
                                       (fn [member-group-pair-guesses]
                                         (conj
                                          (vec
                                           (remove #(= (first %) v)
                                                   member-group-pair-guesses))
                                          [v (member->group-name v)])))
                                (swap! !member-group-pair-guesses
                                       conj [v (member->group-name v)]))
                              (swap! !keywords conj v))
                            (if ((set memes) v)
                              (if ((set history) v)
                                (do
                                  (swap! !history
                                         (fn [history]
                                           (conj (vec (remove #(= % v) history)) v))))
                                (do
                                  (if (= most-recent-guess answer)
                                    (reset! !best nil)
                                    (swap! !best #(if (or (nil? %)
                                                          (< (get-in meme-map [v :rank]) %))
                                                    (get-in meme-map [v :rank])
                                                    %)))
                                  (swap! !history conj v)))
                              (reset! !alert-word v)))))))
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
                             (reset! !most-recent-guess v)
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
                                   "pointer-events-none"))))))
                    (when alert-word
                      (div
                        (tw "text-xs text-red-500")
                        (text "* " alert-word ": 아직 등록되지 않은 밈/별명/인물입니다."))))
                  (div
                    (tw "text-xs text-gray-600")
                    (text "* 정답에 가까워질수록 단어를 공개할 수 있는 힌트를 얻게 됩니다."))))
              (when finished
                (div
                  (tw "ml-4 font-bold mb-2 flex")
                  (cond won
                        (div
                          (tw "text-green-500")
                          (text "킹아! 정답입니다!")
                          #_(if hard-mode
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
              (let [desc (get-in meme-map [answer :desc])]
                (div
                  (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm mb-2 text-sm"
                      ;; (when hard-mode
                      ;;   "border-red-800")
                      )
                  (div
                    (tw "flex")
                    (div
                      (tw "w-fit h-5 border-[*] mb-1")
                      (if finished
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
                    (let [member (get-in meme-map [answer :topic])]
                      (cond
                        (or won gave-up
                            (u/in? (flatten member-group-pair-guesses) member))
                        (div (text (str "[" member
                                        "]")))
                        (u/in? (flatten member-group-pair-guesses)
                               (member->group-name member))
                        (div (text (str "[" (member->group-name member)
                                        "]")))))
                    (div (tw "grow"))
                    (ui/button
                      (e/fn [])
                      (tw "btn-[* xs] mb-1 rounded-sm font-normal border-[black 1] hover:border-[black 1] ml-2")
                      (text (str "누적 힌트 개수:"))
                      (span
                       (tw (when (< 0 hint-count)
                             "font-bold text-green-400 bg-gray-800")
                           "p-1 rounded mx-[-4px]")
                       (text hint-count))))
                  (if finished
                    (div
                      (tw "font-bold mr-1")
                      (text desc))
                    (do
                      (let [words (split-include-delimiter desc (vec keywords)
                                                           ;; (concat
                                                           ;;  reveals keywords)
                                                           )]
                        (e/for [[s i] (map vector
                                           words
                                           (range))]
                          (if (even? i)
                            (e/for [word (str/split s #" ")]
                              (span
                               (tw "text-transparent bg-gray-300 mr-1 hover:cursor-pointer"
                                   (if (< 0 hint-count)
                                     "hover:bg-green-500"
                                     "hover:bg-gray-700"))
                               (style {"filter" "blur(2px)"})
                               (on "click"
                                   (e/fn [e]
                                     (when (< 0 hint-count)
                                       (swap! !keywords conj word)
                                       (swap! !used-hint-count inc))))
                               (text
                                (apply str (repeat (count word) "---")))))
                            (span
                             (tw "font-bold mr-1"
                                 (when (= i 1) "ml-[-4px]"))
                             (text s)))))))
                  (when view-related
                    (div
                      (tw "my-1")
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
                                  (str "\n[" (get-in meme-map [s :topic]) "]")))))
                            (text s))
                          (div
                            (tw "grow"))
                          (div
                            (text (subs (str score) 0 5)))))))
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
              (HistoryDisplay. game-state meme-map most-recent-guess history member-group-pair-guesses play-flags reset-fn)
              )
            (Q&A.)))
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
