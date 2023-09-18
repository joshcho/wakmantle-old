(ns app.meme
  #?(:clj (:import
           [java.util Collections ArrayList Random Collection]
           ;; (clojure.core.cache LRUCache)
           ))
  (:require
   contrib.str
   ;; #?(:clj [datascript.core :as d])
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
      [clojure.tools.macro :refer [macrolet symbol-macrolet]])
   #?(:clj
      [app.data :refer [우왁굳 아이네 징버거 비챤 릴파 주르르 고세구 gomem1 gomem2] :as data])
   [app.printer :as p]
   [app.kr :as kr]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.core.matrix :as mat]
   [malli.core :as m]
   ;; [clojure.core.cache :as cache]
   #?(:clj
      [clojure.core.cache.wrapped :as w]
      )
   ;; #?(:clj
   ;;    [clojure.core.cache :as cache])
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

#?(:clj
   (do

     (def extra-meme-description-map
       (->> (str/split data/raw-composed-pairs-text
                       #"\n\n")
         (map (fn [pair-text]
                (mapv str/trim (str/split pair-text #"\n" 2))))
         (into {})))
     (defn get-embeddings [strings]
       (assert (coll? strings))
       (->> (api/create-embedding
             {:model "text-embedding-ada-002"
              :input strings})
         :data
         (sort-by :index)
         (map :embedding)))
     (defonce get-embeddings-memo (memoize get-embeddings))))

#?(:clj
   (do
     (def owner-meme-info-map
       (letfn [(handle-colon [streamer-var]
                 (->> (str/split streamer-var (re-pattern "\n+"))
                   (map #(str/split % (re-pattern ": ") 2))
                   (filter
                    (fn [x]
                      (= (count x) 2)))
                   (map (fn [[x y]]
                          [(str/trim x) y]))))
               (handle-gomem [x]
                 (let [[s1 s2] x]
                   (->> (str/split (str s1 "\n" s2)
                                   (re-pattern "\n+"))
                     (map #(str/split % (re-pattern ": ") 2))
                     (filter
                      (fn [x]
                        (= (count x) 2)))
                     (map (fn [[x y]]
                            [(str/trim x) y])))))]
         (->> (merge
               {nil (merge (dissoc extra-meme-description-map
                                   "아이네" "징버거" "릴파" "주르르" "고세구" "비챤")
                           {"IKU0019"         "하쿠의 짭멤."
                            "왁빡오"          "머리에 국자가 달린 왁파고의 짭멤."
                            "이덕구 할아버지" "이덕수 할아바이의 짭멤."
                            "헬로석"          "해루석의 짭멤."
                            "비스니즈킴"      "비즈니스킴의 짭멤."
                            "문신"            "풍신의 짭멤."
                            "잡리더"          "프리터의 짭멤."
                            "쿠키킹"          "히키킹의 짭멤. 우왁굳이 히키킹 킹받게 할려고 뽑았다."
                            "단답벌"          "단답벌레의 짭멤."
                            "노스트라담 후드" "노스트라투 호드의 짭멤"
                            "진짜 부정형인간" "부정형인간의 짭멤."
                            "동팡밍방상"      "동글동글 빔을 맞은 도파민 박사의 짭멤."
                            "공개소녀"        "소름돋는 싱크로율을 가진 비밀소녀의 짭멤."
                            "퀑민"            "권민의 짭멤."
                            "엔젤"            "우왁굳의 와이프, 김수현 아나운서를 지칭하는 말. 결혼 전부터 여자친구로써 엔젤이라고 불려져왔다."
                            "엔하"            "엔젤 하이라는 뜻. 방송 중 엔젤님이 등장하면 채팅창에 등장한다."
                            "왁하"            "우왁굳 하이라는 뜻."})}
               {"이세계아이돌" (select-keys extra-meme-description-map
                                            ["아이네" "징버거" "릴파" "주르르" "고세구" "비챤"])
                "고정멤버"
                (->> ["뢴트게늄" "해루석" "소피아" "프리터" "캘리칼리 데이비슨" "비밀소녀" "이덕수 할아바이" "비즈니스 킴" "하쿠0089" "독고혜지" "도파민 박사" "카르나르 융터르" "히키킹" "풍신" "곽춘식" "권민" "김치만두번영택사스가" "노스페라투 호드" "단답벌레" "부정형인간" "왁파고"]
                  (map (fn [meme-name]
                         [meme-name ""]))
                  (into {}))
                "고멤아카데미"
                (->> ["시리안 레인" "아마데우스최" "미스" "발렌타인" "닌닌" "빅토리" "캡틴 설리반" "데스해머 쵸로키" "미미짱짱세용" "수셈이" "진희" "철도왕 길버트" "불곰" "사랑전도사 젠투" "정삼품 통정대부 대곡" "만강준" "크리즈" "스바스키 나라바" "아이 쓰께끼"]
                  (map (fn [meme-name]
                         [meme-name ""]))
                  (into {}))}
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
               (u/map-vals handle-gomem gomem2))
           (mapv (fn [[owner meme-description-pairs]]
                   (let [embeddings (get-embeddings-memo
                                     (map (fn [[meme-name description]]
                                            (str meme-name
                                                 (when owner
                                                   (str " [" owner
                                                        (cond
                                                          (contains? #{"아이네" "징버거" "릴파" "주르르" "고세구" "비챤"}
                                                                     owner)
                                                          ",이세계아이돌"
                                                          (not= owner "우왁굳")
                                                          ",고정멤버")
                                                        "]"))
                                                 ":"
                                                 description))
                                          meme-description-pairs))]
                     [owner (->> (map (fn [[meme-name description] embedding]
                                        [meme-name {:description description
                                                    :embedding   embedding
                                                    :owner       owner}])
                                      meme-description-pairs embeddings)
                              (into {}))])))
           (into {}))))

     #_(assert (m/validate [:map-of :string
                            [:map-of :string
                             [:map
                              [:description :string]
                              ;; keep this as :any, :int takes too long
                              [:embedding [:vector :any]]
                              ;; [:owner :string]
                              ]]]
                           owner-meme-info-map)
               "Something wrong with owner-meme-info-map")

     (def all-meme-info-map
       (apply merge (vals owner-meme-info-map)))

     #_(assert (m/validate [:map-of :string
                            [:map
                             [:description :string]
                             ;; keep this as :any, :int takes too long
                             [:embedding [:vector :any]]
                             ;; [:owner :string]
                             ]]
                           all-meme-info-map)
               "Something wrong with all-meme-info-map")))

#?(:clj
   (do
     (defn seeded-shuffle
       [coll seed]
       (let [al  (java.util.ArrayList. coll)
             rng (java.util.Random. seed)]
         (java.util.Collections/shuffle al rng)
         (vec (.toArray al))))
     (defn get-guess-score-map [answer]
       (letfn [(cosine-similarity [v1 v2]
                 (let [magnitude (fn [v] (Math/sqrt (mat/dot v v)))
                       dot-prod  (mat/dot v1 v2)
                       mag1      (magnitude v1)
                       mag2      (magnitude v2)]
                   (/ dot-prod (* mag1 mag2))))]
         (let [answer-embedding (get-in all-meme-info-map [answer :embedding])
               get-similarity   (fn [guess-embedding]
                                  (cosine-similarity answer-embedding
                                                     guess-embedding))
               guess-similarity-pairs
               (map (fn [[meme-name {:keys [embedding]}]]
                      (assert embedding)
                      [meme-name (get-similarity embedding)])
                    ;; we remove the answer for max calculation later
                    (dissoc all-meme-info-map answer))
               similarities     (map second guess-similarity-pairs)
               max-similarity   (let [max-similarity (apply max similarities)]
                                  (+ max-similarity
                                     (/ (- 1 max-similarity) 20)))
               min-similarity   (apply min similarities)
               diff-similarity  (- max-similarity min-similarity)]
           (->> (map
                 (fn [[meme-name similarity]]
                   (let [score (* 100 (/ (- similarity min-similarity)
                                         diff-similarity))
                         owner (get-in all-meme-info-map [answer :owner])
                         adjusted-score
                         (if (= owner meme-name)
                           (max score (+ 65 (* 5 (rand))))
                           score)]
                     [meme-name adjusted-score]))
                 guess-similarity-pairs)
             ;; we add the answer back in
             (into {answer 100.0})))))))

(defstate ctr-state
  {global-state {:ctr/host            :server
                 _n-todays-answers    9
                 :_answer-seed        42
                 ;; (int (* 1000 (rand)))
                 :_today-number       (e/server (- (int (quot (+ e/system-time-secs
                                                                 (* 9 60 60))
                                                              (* 24 60 60)))
                                                   19618))
                 _all-meme-names      (e/server (keys all-meme-info-map))
                 :_todays-answers     (e/server
                                       (->> (seeded-shuffle
                                             (keys (filter (fn [[k v]]
                                                             (and (:owner v)
                                                                  (not= (:owner v) "고정멤버")
                                                                  (not= (:owner v) "이세계아이돌")
                                                                  (not= (:owner v) "고멤아카데미")))
                                                           all-meme-info-map))
                                             answer-seed)
                                         (drop (* n-todays-answers today-number))
                                         (take n-todays-answers)))
                 :_guess-score-maps   (e/server (mapv get-guess-score-map todays-answers))
                 :_guess-rank-maps    (mapv (fn [m]
                                              (->> (map
                                                    (fn [[meme-name _] rank]
                                                      [meme-name rank])
                                                    (reverse (sort-by second m))
                                                    (map inc (range)))
                                                (into {})))
                                            guess-score-maps)
                 :_guess-to-suggestion-cache
                 nil
                 :_meme-info-map      nil
                 :_autocomplete-count 10}
   session-state
   {:ctr/host    :client
    answer-state {answer-idx            0
                  _answer               (nth (:todays-answers global-state)
                                             answer-idx)
                  :_answer-descriptions (e/server (mapv #(get-in all-meme-info-map [% :description])
                                                        (:todays-answers global-state)))
                  _answer-description   (nth answer-descriptions answer-idx)
                  _answer-owner
                  (e/server (get-in all-meme-info-map [answer :owner]))
                  _guess-score-map      (nth (:guess-score-maps global-state)
                                             answer-idx)
                  _guess-rank-map       (nth (:guess-rank-maps global-state)
                                             answer-idx)
                  :_on-last-idx         (= answer-idx (dec n-todays-answers))
                  :_on-first-idx        (= answer-idx 0)}
    play-state   {:revealed-indices []
                  play-flags        {:won       false
                                     :gave-up   false
                                     :_finished (or won gave-up)}
                  ;; scores            []
                  ;; current-score     nil
                  view-related      false
                  :ctr/expr
                  (do
                    (when (= recent-guess answer)
                      (reset! (:!won play-flags) true))
                    #_(when n-todays-answers
                        (reset! !scores (vec (repeat n-todays-answers nil))))
                    #_(when (and (nil? current-score) (:finished play-flags))
                        (reset! !current-score (->> (+ (- 20
                                                          (if (:won play-flags) 0 10)
                                                          (count past-guesses))
                                                       1 ; 1 for one final guess
                                                       (if (:won play-flags)
                                                         current-hint-count
                                                         0))
                                                 (max 0)
                                                 (min 20))))
                    #_(when current-score
                        (swap! !scores
                               (fn [scores]
                                 (when (nil? (nth scores answer-idx))
                                   (assoc scores answer-idx current-score))))))}
    guess-state  {hint-state
                  {word-hovered          false
                   :_earned-hint-count   (if best-guess-rank
                                           (cond (> best-guess-rank 256) 0
                                                 (> best-guess-rank 192) 1
                                                 (> best-guess-rank 128) 2
                                                 (> best-guess-rank 64)  3
                                                 (> best-guess-rank 32)  4
                                                 (> best-guess-rank 16)  5
                                                 (> best-guess-rank 8)   6
                                                 (> best-guess-rank 4)   7
                                                 (> best-guess-rank 2)   8
                                                 :else                   9)
                                           0)
                   :used-hint-count      0
                   :_answer-description-word-count
                   (count (str/split answer-description #" "))
                   :_starting-hint-count (cond
                                           (< 40 answer-description-word-count)
                                           6
                                           (< 25 answer-description-word-count)
                                           5
                                           (< 15 answer-description-word-count)
                                           4
                                           (< 8 answer-description-word-count)
                                           3
                                           :else
                                           2)
                   _current-hint-count   (max 0 (- (+ starting-hint-count
                                                      earned-hint-count)
                                                   used-hint-count))}
                  recent-guess    nil
                  past-guesses    []
                  best-guess-rank nil
                  :ctr/expr
                  (do
                    (when recent-guess
                      (when-not (or (= recent-guess answer)
                                    (= best-guess-rank 1))
                        (swap! !best-guess-rank
                               (fn [best-guess-rank]
                                 (if (nil? best-guess-rank)
                                   (get guess-rank-map recent-guess)
                                   (min best-guess-rank (get guess-rank-map recent-guess))))))
                      (swap! !past-guesses
                             (fn [past-guesses]
                               (if (u/in? past-guesses recent-guess)
                                 (conj
                                  (vec (remove (partial = recent-guess) past-guesses))
                                  recent-guess)
                                 (conj past-guesses recent-guess))))))}
    input-state  {input-value ""
                  :ctr/do-expr
                  #?(:clj
                     (do
                       #_(defmacro pred-lookup-or-miss [pred cache k & body]
                           `(if ~pred
                              (w/lookup-or-miss
                               ~cache ~k
                               (constantly ~@body))
                              (do
                                ~@body)))
                       (def completions-cache
                         (w/lru-cache-factory {} :threshold 128)))
                     :cljs nil)
                  _input-value-completions
                  (e/server
                   (if (= (count input-value) 1)
                     (w/lookup-or-miss
                      completions-cache input-value
                      (constantly
                       (when (and all-meme-names
                                  (not-empty input-value))
                         (some->>
                          all-meme-names
                          (mapv #(vector % (kr/kr-match-index input-value %)))
                          (filter second)
                          (sort-by second)
                          (map first)
                          (take (:autocomplete-count global-state))
                          vec))))
                     (when (and all-meme-names
                                (not-empty input-value))
                       (some->>
                        all-meme-names
                        (mapv #(vector % (kr/kr-match-index input-value %)))
                        (filter second)
                        (sort-by second)
                        (map first)
                        (take (:autocomplete-count global-state))
                        vec))))}}}
  CtrExprs)


(e/defn Main []
  (e/client
   (div
     (CtrExprs.)
     ;; (div (text ctr-state))
     ;; (div (text answer-idx))
     ;; (div (text (:todays-answers global-state)))
     ;; (div (text input-value))
     ;; (div (text input-value))
     ;; (div (text (e/server (take 10 all-meme-names))))
     ;; (div (text (e/server input-valoo)))
     ;; (div (text input-value-completions))
     ;; (div (text scores))
     ;; (div (text play-flags))
     ;; (div (text past-guesses))
     ;; (p/peek answer-state)
     ;; (p/peek global-state)
     ;; (div (text best-guess-rank))
     ;; (p/peek ctr-state)
     ;; (div (text answer-description))
     #_(div (text
             (str input-value-completions)))
     (div
       (tw "mx-auto mt-14 sm:mt-12 max-w-md")
       (div
         (tw "mx-auto w-fit my-2 text-lg font-bold")
         (text "오늘의 왁맨틀 - 왁타버스 밈 추측 게임"))
       ;; (div (text scores))
       ;; (div (text current-score))
       ;; (div (text (count past-guesses)))
       (div
         (tw "mx-4 my-2 border-[black 2] p-2")
         (div
           (letm {temp           nil
                  alert-word     nil
                  input-reset-fn (fn []
                                   (reset! !temp nil)
                                   (reset! !input-value "")
                                   (reset! !alert-word nil))}
             (div (tw "flex")
               (u/input*
                (or temp input-value)
                (e/fn [v]
                  (reset! !temp nil)
                  (reset! !input-value v))
                (props {:placeholder "맞춰보세요"})
                (tw "input-[* sm] rounded-md pl-2 mb-1")
                (arrows !temp input-value-completions)
                (enter
                 (e/fn [v]
                   (input-reset-fn)
                   (if (e/server (u/in? all-meme-names v))
                     (reset! !recent-guess v)
                     (reset! !alert-word v))))))
             (when (and (not-empty input-value)
                        (not-empty input-value-completions))
               (div
                 (tw "absolute menu bg-base-200 rounded-box absolute z-50 mt-1"
                     "border-[neutral-content 2]")
                 (dropdown
                  (if (empty? input-value-completions)
                    ["검색 결과 없음"]
                    input-value-completions)
                  (e/fn [v]
                    (when (not-empty input-value-completions)
                      (input-reset-fn)
                      (reset! !recent-guess v)))
                  (tw "rounded-sm p-1"
                      (when (= (.-textContent dom/node) temp)
                        "bg-neutral-content")
                      (when (empty? input-value-completions)
                        (tw "hover:bg-base-200"
                            "pointer-events-none"))))))
             (when alert-word
               (div
                 (tw "text-xs text-red-500 my-[2px]")
                 (text "* " alert-word ": 아직 등록되지 않은 밈/별명/인물입니다."))))
           (div
             (tw "text-xs text-gray-600")
             (text "* 정답에 가까워질수록 단어를 공개할 수 있는 힌트를 얻게 됩니다."))))
       (let [{:keys [finished won gave-up]} play-flags]
         (when finished
           (div
             (tw "ml-4 font-bold mb-2 flex")
             (cond won     (div (tw "text-green-500")
                             (text
                              (str
                               (cond (and (= (count past-guesses) 1)
                                          (= (:used-hint-count hint-state) 0))
                                     "홀리왁? "
                                     (< (count past-guesses) 5)
                                     "킹아! ")
                               "정답입니다!")))
                   gave-up (div (tw "text-red-500")
                             (text "아쉽탈")))
             (ui/button
               (e/fn [] (swap! !view-related not))
               (tw "btn-[* xs] rounded-sm font-normal border-[black 1] hover:border-[black 1] ml-2")
               (text
                (if view-related
                  "관련 밈 끄기"
                  "관련 밈 보기")))))
         (div (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm mb-2 text-sm")
           (div (tw "flex")
             (div (tw "w-fit h-5 border-[*] mb-1")
               (if finished
                 (div (tw "font-bold mr-1 text-green-500")
                   (text answer))
                 (e/for [i (range (count answer))]
                   (let [s (let [s (str (nth answer i))]
                             (if (= s " ")
                               s
                               "_"))]
                     (span
                      (tw "mr-1")
                      (text s))))))
             (when (and finished answer-owner)
               (div (text (str "[" answer-owner "]"))))
             (div
               (tw "grow"))
             (div
               (tw "w-24 p-[2px] pt-[1px] mt-[-4px] mr-[-2px] rounded-sm pr-[10px]")
               (div
                 (tw "flex m-[-4px] relative")
                 (when current-hint-count
                   (when (< 5 current-hint-count)
                     (div
                       (tw "absolute top-[7px] right-[2px] font-bold text-xs")
                       (text current-hint-count)))
                   (e/for [i (range 5)]
                     (let [wave-amount (Math/cos (/ (* (mod (Math/round (/ e/system-time-ms 10))
                                                            360)
                                                       Math/PI) 180))
                           offset      (* 0.15 wave-amount)]
                       (svg
                        (props {:viewBox "0 0 6 6"})
                        (tw "w-6 h-6 mr-[-6px]")
                        (rect
                         (tw "shadow-sm"
                             (if (or (and (= i (dec 5))
                                          (< 0 current-hint-count))
                                     (and (<= current-hint-count 5)
                                          (<= (- 5 current-hint-count) i)))
                               "visible"
                               "invisible"))
                         (props {:x      (- 2 (/ offset 2))
                                 :y      (- 2 (/ offset 2))
                                 :rx     0.5
                                 :width  (+ 3.5 offset)
                                 :height (+ 3.5 offset)
                                 :style
                                 {:fill
                                  (if (and (= i (- 5 current-hint-count))
                                           word-hovered)
                                    "rgb(20 177 64)"
                                    "rgb(40 205 105)")}})
                         (when (and (= i (- 5 current-hint-count))
                                    word-hovered)
                           (style {"filter" "blur(0.1px)"})))))))))
             #_(ui/button
                 (e/fn [])
                 (tw "btn-[* xs] mb-1 rounded-sm font-normal border-[black 1] hover:border-[black 1] ml-2")
                 (text (str "힌트 개수:"))
                 (span
                  (tw "p-1 rounded mx-[-4px]"
                      (when (< 0 current-hint-count)
                        "font-bold text-green-400 bg-gray-800"))
                  (text current-hint-count))))
           (if finished
             (div (tw "font-bold mr-1")
               (text answer-description))
             (let [revealed-indices-set (set (:revealed-indices play-state))]
               (div
                 (if (dom/Hovered?.)
                   (reset! !word-hovered true)
                   (reset! !word-hovered false))
                 (e/for [[word idx] (map vector
                                         (str/split answer-description #" ")
                                         (range))]
                   (span
                    (tw
                     "font-bold mr-1" (when (= idx 0) "ml-[-2px]")
                     (when-not (contains? revealed-indices-set idx)
                       "text-transparent bg-gray-300 mr-1 hover:cursor-pointer")
                     (when-not (contains? revealed-indices-set idx)
                       (if (< 0 current-hint-count)
                         "hover:bg-green-500"
                         "hover:bg-gray-700")))
                    (when-not (contains? revealed-indices-set idx)
                      (style {"filter" "blur(2px)"})
                      (on "click"
                          (e/fn [e]
                            (when (< 0 current-hint-count)
                              (swap! (:!revealed-indices play-state)
                                     conj idx)
                              (swap! (:!used-hint-count hint-state)
                                     inc)))))
                    (text
                     (if-not (contains? revealed-indices-set idx)
                       (apply str (repeat (count word) "---"))
                       word)))))))
           (when view-related
             (div (tw "my-1")
               (e/for [[meme-name score]
                       (->> guess-score-map
                         (sort-by second)
                         reverse
                         rest
                         (take 12))]
                 (div (tw "flex gap-2")
                   (div (tw "cursor-pointer")
                     (when (dom/Hovered?.)
                       (div (tw "absolute bg-neutral-content ml-12 mt-6 rounded-md"
                                "p-2 text-sm mr-6 sm:mr-0"
                                "max-w-sm drop-shadow-lg whitespace-pre-line")
                         (let [description
                               (e/server
                                (get-in all-meme-info-map [meme-name :description]))]
                           (when (not-empty description)
                             (text
                              (str
                               description
                               (let [owner (e/server (get-in all-meme-info-map [meme-name :owner]))]
                                 (when-not (or (empty? owner) (nil? owner))
                                   (str
                                    "\n[" owner "]")))))))))
                     (text meme-name))
                   (div (tw "grow"))
                   (div (text
                         (if (= score 100)
                           "100.00"
                           (subs (str score) 0 5))))))))))
       (when (not-empty past-guesses)
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
           (e/for [[meme-name idx]
                   (let [indexed-pairs
                         (map vector past-guesses (map inc (range)))]
                     ;; push last to top
                     (u/concatv (list (last indexed-pairs))
                                (->> (drop-last indexed-pairs)
                                  (sort-by
                                   (fn [[meme-name _]]
                                     (get guess-score-map meme-name)))
                                  reverse)))]
             (let [first? (= idx (count past-guesses))]
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
                       (if (= meme-name answer)
                         (tw "text-green-500 font-bold")
                         (when first?
                           (tw "text-red-400 font-bold")))
                       (text meme-name))
                     (when (dom/Hovered?.)
                       (let [description
                             (e/server (get-in all-meme-info-map
                                               [meme-name :description]))]
                         (when (not-empty description)
                           (div
                             (tw "absolute bg-neutral-content ml-5 mt-1 rounded-md p-2 text-sm"
                                 "max-w-sm drop-shadow-lg whitespace-pre-line")
                             (text (str
                                    description
                                    (let [owner (e/server (get-in all-meme-info-map [meme-name :owner]))]
                                      (when-not (or (empty? owner) (nil? owner))
                                        (str
                                         "\n[" owner "]")))))))))))
                 (div
                   (tw "flex w-24 sm:w-36")
                   (div
                     (tw "grow")
                     (text (subs (str (get guess-score-map meme-name))
                                 0 5)))
                   (div (text (get guess-rank-map meme-name)))))))
           #_(e/for [[member group] (reverse member-group-pair-guesses)]
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
                     (text "-")))))
           ))
       (div (tw "flex")
         (div (tw "grow"))
         (div (tw "text-sm text-gray-500 mt-[-6px] mr-[19px] mb-[-8px]")
           (text
            "(" (inc answer-idx) "/" n-todays-answers ")"
            #_(let [n (inc (:today-number global-state))]
                (cond (= n 1) "첫" (= n 2) "두"   (= n 3) "세"
                      (= n 4) "네" (= n 5) "다섯" true    n))
            ;; "번째 왁맨틀"
            )))
       (div
         (tw "flex gap-2 ml-4 mt-[-6px]")
         (when-not (:finished play-flags)
           (dom/button
             (dom/on! "click"
                      (fn []
                        ;; js/alert has some weird behavior
                        (reset! !best-guess-rank nil)
                        (reset! (:!gave-up play-flags) true)))
             (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md")
             (text "포기")))
         (if (and (:finished play-flags)
                  (not= answer-idx (dec n-todays-answers)))
           (ui/button
             (e/fn []
               (swap! (:!answer-idx answer-state) inc)
               (reset! (:!won play-flags) false)
               (reset! (:!gave-up play-flags) false)
               (reset! (:!used-hint-count hint-state) 0)
               (reset! !past-guesses [])
               (reset! !recent-guess nil)
               (reset! !view-related nil)
               ;; (reset! !current-score nil)
               (reset! !best-guess-rank nil)
               (reset! (:!revealed-indices play-state) []))
             (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md"
                 "border-[primary 2] hover:border-black")
             (text "다음 문제"))
           (when (and (:finished play-flags)
                      (= answer-idx (dec n-todays-answers)))
             (div
               (tw "text-sm text-gray-600 ml-1 mt-[-1px]")
               (text "내일의 왁맨틀: "
                     (let [today-secs (- (* 3600 24)
                                         (mod (+ (e/server
                                                  (int e/system-time-secs))
                                                 (* 3600 9))
                                              (* 3600 24)))]
                       (str
                        (int (/ today-secs 3600))
                        ":"
                        (let [mins (mod (int (/ today-secs 60)) 60)]
                          (str
                           (when (= (count (str mins)) 1)
                             "0")
                           mins))
                        ":"
                        (let [secs (mod (int today-secs) 60)]
                          (str
                           (when (= (count (str secs)) 1)
                             "0")
                           secs))))))))))
     (Q&A.))))

(e/defn Q&A []
  (div
    (tw "max-w-lg mx-auto mt-4 text-sm px-4 sm:px-0")
    (div
      (text "Q & A"))
    (e/for [[s1 s2] [["왁맨틀은 무엇인가요?"
                      "왁맨틀은 오늘의 왁타버스 밈을 맞히는 게임입니다. 기존 꼬맨틀 게임과 비슷하지만, 정답에 가까워질수록 부분적으로 설명을 공개할 수 있습니다."]
                     ["어떤 밈이 포함되어 있나요?"
                      (str "우왁굳, 이세돌, 고멤 관련 밈과 별명 총 "
                           (e/server (count all-meme-info-map))
                           "가지가 포함되어 있습니다.\n예) 레게노, 징버거, 미웡, 선량한 시민")]
                     ["정답은 어떻게 맞추나요?"
                      "유사성 순위가 높은 밈을 맞춰 힌트를 얻으세요. 순위가 높을수록 힌트를 추가로 얻게 됩니다. 순위가 256 이하일 경우 1개, 196 이하는 2개, 128 이하는 3개 등으로 얻게 됩니다 (최대 9개). 그리고 시작할 때 설명의 길이에 따라 힌트를 얻습니다."]
                     ["유사도는 무엇인가요?"
                      "왁맨틀에서 추측한 밈과 정답 밈의 설명이 의미맥락적으로 얼마나 비슷한지 정도를 백분율로 계산한 점수입니다. 숫자가 클수록 유사한 정도가 크다고 이해할 수 있습니다. 유사도 측정을 위해 사용한 밈 설명은 나무위키를 기반하였습니다."]
                     ["소스 코드를 볼 수 있나요?"
                      "네 여기있습니다."]
                     ["특정 밈이 없어요!"
                      "꼭 들어가야할 밈이나 고쳐야 할 버그가 있다면 깃헙이나 X로 연락주세요."]]]
      (dom/b
       (tw "mt-2")
       (text s1))
      (div
        (tw "sm:text-xs mb-2 whitespace-pre-line")
        (if (= s2 "네 여기있습니다.")
          (a
           (props {:href   "https://github.com/joshcho/wakmantle"
                   :target "_blank"})
           (tw "text-blue-600 hover:text-blue-800 cursor-pointer")
           (text s2))
          (text s2))))
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
                  :width  "30px"})))))))

#_(e/def member->group-name
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
