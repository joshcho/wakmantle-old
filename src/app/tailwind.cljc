(ns app.tailwind
  #?(:cljs (:require-macros [app.tailwind :refer [tw]]))
  (:require
   [hyperfiddle.electric-dom2 :as dom]
   [app.utils :refer [subs*]]        ; safe subs with negative indices
   [clojure.string]
   [instaparse.core :as insta]))


;; Note: `word` here is a non-bracketed non-whitespace string.

(def tailwind-parser
  (insta/parser
   "S = (item <space>)* item
item = (no-group | grouped)
no-group = word
grouped = (colon-prefix | dash-prefix) <'['> ((subitem <space>)* subitem)? <']'>
colon-prefix = (('[' colon-prefix-word ']' | colon-prefix-word) ':') +
colon-prefix-word = #'[^:\\[\\]\\s]+'
dash-prefix = (('[' colon-prefix-word ']' | colon-prefix-word) ':')* #'[^-:\\[\\]\\s]+' '-'
subitem = (word | itself)
word = #'[^\\[\\]\\s]+'
itself = '*'
space = #'\\s+'
"))

(defn on-args [f]
  (fn [& args]
    (f args)))

(def tailwind-valid-dash-group-prefixes
  ["border" "grid" "flex" "input" "btn" "font" "text" "range" "input" "textarea"])
(defn tailwind-generator [tree]
  (->> tree
       (insta/transform
        {:word              str
         :colon-prefix-word str
         :subitem           (on-args first)
         :item              (on-args first)
         :no-group          str
         :grouped           (fn [& args]
                              (let [prefix-type     (ffirst args)
                                    prefix          (apply str (rest (first args)))
                                    contains-itself (some #(= (first %) :itself)
                                                          (rest args))
                                    subitems        (remove #(= (first %) :itself)
                                                            (rest args))]
                                (if (and (= prefix-type :dash-prefix)
                                         (= (count subitems) 1)
                                         (not
                                          (re-matches
                                           (re-pattern
                                            (str
                                             ".*("
                                             (clojure.string/join
                                              "|"
                                              tailwind-valid-dash-group-prefixes)
                                             ")-$"))
                                           prefix)))
                                  (str prefix "[" (first subitems) "]")
                                  (clojure.string/join
                                   " "
                                   (concat
                                    (when contains-itself
                                      (list (subs* prefix 0 -1)))
                                    (map (fn [subitem]
                                           (str prefix
                                                subitem))
                                         subitems))))))
         :S                 (on-args #(clojure.string/join " " %))
         })))

(defn tailwind-compiler [string]
  (tailwind-generator
   (tailwind-parser string)))

(comment
  (tailwind-compiler "focus:hover:grid-[* p-4 rounded-2xl font-mono]")
  (tailwind-compiler "[&>*]:[p-4 rounded-2xl font-mono]")
  (tailwind-compiler "grid-[* p-4 rounded-2xl font-mono]")
  (tailwind-compiler "grid hover:x")
  (tailwind-compiler "hover:[p-4 rounded-2xl font-mono]")
  (tailwind-compiler "grid-[]")
  (tailwind-compiler "hover:[]"))

#?(:clj
   (defmacro -tw
     "Output a tailwind class string, using `tailwind-compiler`. If args are strings, then parsed at compile-time. Otherwise, parsed at run-time."
     [& args]
     `(clojure.string/join
       " "
       (remove
        nil?
        (list
         ~@(map (fn [arg]
                  (if (string? arg)
                    (let [arg# (clojure.string/trim arg)]
                      (tailwind-compiler arg#))
                    `(when ~arg
                       (tailwind-compiler ~arg))))
                args))))))

#?(:clj
   (defmacro tw
     ;; for some reason, first arg here cannot be nil...?
     ;; but it's okay as second arg
     [& args]
     `(dom/props {:class (-tw ~@args)})))
