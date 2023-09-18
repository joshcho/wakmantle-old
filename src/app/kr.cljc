(ns app.kr
  (:require
   [app.utils :as u :refer [cond-let cmt enter arrows mb-> mb->> mb->>> dropdown
                            >defn]]))

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
