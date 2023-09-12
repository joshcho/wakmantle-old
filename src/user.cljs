(ns ^:dev/always user ; Electric currently needs to rebuild everything when any file changes. Will fix
  (:require
   app.meme
   hyperfiddle.electric
   hyperfiddle.electric-dom2))

(def electric-main
  (hyperfiddle.electric/boot ; Electric macroexpansion - Clojure to signals compiler
   (binding [hyperfiddle.electric-dom2/node js/document.body]
     ;; (app.todo-list/Todo-list.)
     ;; (app.reactive-render/ReactiveRenderApp.)
     ;; (app.reactive-render/TestingApp.)
     ;; (app.kondo/Main.)
     ;; (app.forth/Main.)
     (app.meme/Main.)
     )))

(defonce reactor nil)

(defn ^:dev/after-load ^:export start! []
  (assert (nil? reactor) "reactor already running")
  (set! reactor (electric-main
                  #(js/console.log "Reactor success:" %)
                  #(js/console.error "Reactor failure:" %))))

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))
