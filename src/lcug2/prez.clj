(ns lcug2.prez
  (:require [clojure.core.async :as a]
            [clj-http.client :as h]))

;; Lyon Clojure User Group #2 : Transducteurs & asynchronicité


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transducteurs
;; CLJ : "A composable way to build algorithmic transformations."
;; * générique : agnostique vis à vis du conteneur
;; * composable : chaînable à la manière des transformateurs de lazy seqs
;; * performant : éviter la création de conteneurs intermédiaires au cours de la transformation

;; Asynchrone
;; CLJ : "Découplage du flux d'exécution"
;; Un moyen pour une fin : ne pas bloquer les threads.
;; * back : optimisation des threads
;; * front : réactivité de l'interface
;; * cljs : obligation imposée par le modèle d'exécution de la plate-forme


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1- Construction et utilisation                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Les séquences de valeurs ont vocation à réduites, on peut donc reporter la transformation à l'étape de réduction.
;; Déplaçons la transformation de la séquence dans la fonction de réduction.

(defn map+ [f xs]
  (reduce (fn [r x]
            (+ r (f x)))
          0 xs))

(comment
  (map+ inc (range 10))
  (reduce + 0 (map inc (range 10))))


(defn filter+ [p xs]
  (reduce (fn [r x]
            (if (p x) (+ r x) r))
          0 xs))

(comment
  (filter+ odd? (range 10))
  (reduce + 0 (filter odd? (range 10))))


(defn mapcat+ [f xs]
  (reduce (fn [r x]
            (reduce + r (f x)))
          0 xs))

(comment
  (mapcat+ range (range 10))
  (reduce + 0 (mapcat range (range 10))))


;; Factorisons la somme

(defn sum [xf xs]
  (reduce (xf +) 0 xs))


(defn map+ [f xs]
  (sum (fn [rf]
         (fn [r x]
           (rf r (f x))))
       xs))

(comment
  (map+ inc (range 10))
  (reduce + 0 (map inc (range 10))))


(defn filter+ [p xs]
  (sum (fn [rf]
         (fn [r x]
           (if (p x) (rf r x) r)))
       xs))

(comment
  (filter+ odd? (range 10))
  (reduce + 0 (filter odd? (range 10))))


(defn mapcat+ [f xs]
  (sum (fn [rf]
         (fn [r x]
           (reduce rf r (f x))))
       xs))

(comment
  (mapcat+ range (range 10))
  (reduce + 0 (mapcat range (range 10))))


;; Isolons la transformation
;; Les fonctions que nous manipulons maintenant prennent comme argument une fonction de réduction, et renvoient
;; une nouvelle fonction de réduction qui traduit la transformation à appliquer.

(defn mapper [f]
  (fn [rf]
    (fn [r x]
      (rf r (f x)))))

(defn filterer [p]
  (fn [rf]
    (fn [r x]
      (if (p x) (rf r x) r))))

(defn mapcatter [f]
  (fn [rf]
    (fn [r x]
      (reduce rf r (f x)))))


(def map-inc (mapper inc))
(comment
  (reduce (map-inc conj) [] (range 10))
  (reduce conj [] (map inc (range 10))))
(comment
  (reduce (map-inc +) 0 (range 10))
  (reduce + 0 (map inc (range 10))))


(def filter-odd? (filterer odd?))
(comment
  (reduce (filter-odd? conj) [] (range 10))
  (reduce conj [] (filter odd? (range 10))))
(comment
  (reduce (filter-odd? +) 0 (range 10))
  (reduce + 0 (filter odd? (range 10))))


(def mapcat-range (mapcatter range))
(comment
  (reduce (mapcat-range conj) [] (range 10))
  (reduce conj [] (mapcat range (range 10))))
(comment
  (reduce (mapcat-range +) 0 (range 10))
  (reduce + 0 (mapcat range (range 10))))


;; Ce sont des transformateurs de fonction de réduction, ils sont composables avec la composition de fonction simple.

(def transform-rf
  (comp map-inc
        filter-odd?
        mapcat-range))

(def transform-seq
  (comp (partial mapcat range)
        (partial filter odd?)
        (partial map inc)))

(comment
  (reduce (transform-rf +) 0 (range 10))
  (reduce + 0 (transform-seq (range 10))))


;; Les transducers peuvent faire évoluer un état au cours de la transformation
;;
;; Exemple : map-indexed (élagué)

(defn mapping-indexed [f]
  (fn [rf]
    (let [i (volatile! -1)]
      (fn [r x]
        (rf r (f (vswap! i inc) x))))))

(def pair-index (mapping-indexed vector))

(comment
  (reduce (-> conj
              (pair-index)
              (transform-rf))
          [] (range 10))
  (reduce conj []
          (->> (range 10)
               (transform-seq)
               (map-indexed vector))))


;; reduced peut être utilisé pour signaler au processus de réduction que la transformation est terminée.

(defn taking-while [pred]
  (fn [rf]
    (fn [result input]
      (if (pred input)
        (rf result input)
        (reduced result)))))

(defn <5 [x]
  (< x 5))

(comment
  (reduce (transform-rf ((taking-while <5) conj)) [] (range 10))
  (reduce conj [] (take-while <5 (transform-seq (range 10)))))



;; Depuis la 1.7, les fonctions de transformation de seqs renvoient un transducer lorsqu'elles sont appelées sans
;; la seq. Certaines fonctions de transformation acceptent les transducers.

;; nos transformations précédentes sont disponibles sur étagère
(def transform-rf
  (comp (map inc)
        (filter odd?)
        (mapcat range)))

;; transduce fait un reduce à travers un transducer
(comment
  (transduce transform-rf + (range 10))
  (apply + (transform-seq (range 10))))

;; sequence produit une séquence (paresseuse) en appliquant un transducer à une séquence d'entrée
(comment
  (sequence transform-rf (range 10))
  (transform-seq (range 10)))

;; into accepte une arité supplémentaire pour un transducer
(comment
  (into #{} transform-rf (range 10))
  (into #{} (transform-seq (range 10))))



;; Les transducers sont compatibles avec core.async.

;; Transformation au sein d'un channel
;; Le channel est verrouillé le temps de l'exécution du transducer.
;; Ne pas utiliser pour des traitements longs !
(comment
  (let [c (a/chan 1 (filter odd?))]
    (a/onto-chan c (range 10))
    (a/<!! (a/into [] c))))

;; Utiliser plutôt pipeline et ses variantes (pipeline-blocking, pipeline-async). Le flux de données va être réparti
;; sur n unités de traitement puis recombiné dans un unique channel, en respectant l'ordre d'entrée.
;; Ne pas utiliser avec les stateful transducers !
(defn slow-inc [x]
  (Thread/sleep 1000)
  (inc x))

(comment
  (let [in (a/chan)
        out (a/chan)]
    (a/pipeline 5 out (map slow-inc) in)
    (a/onto-chan in (range 10))
    (time (a/<!! (a/into [] out))))

  ;; wrong
  (let [in (a/chan)
        out (a/chan)]
    (a/pipeline 5 out (comp (map slow-inc) (map-indexed vector)) in)
    (a/onto-chan in (range 10))
    (a/<!! (a/into [] out)))

  ;; correct
  (let [in (a/chan)
        out (a/chan 1 (map-indexed vector))]
    (a/pipeline 5 out (map slow-inc) in)
    (a/onto-chan in (range 10))
    (a/<!! (a/into [] out)))

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Créer un contexte de transduction         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Une réduction est un processus séquentiel : le résultat de l'étape n doit être réinjecté dans l'étape n+1.
;; Les réducteurs NE sont PAS thread-safe. Ils peuvent porter un état mutable et/ou avoir des effets de bord.
;; Ils doivent être utilisés soit en contexte monothreadé (ex: transduce, into, sequence)
;; OU dans un contexte qui garantit que chaque appel au réducteur est exécuté séquentiellement
;; Les atoms et les refs ne sont pas compatibles avec les stateful transducers, à cause des mutations internes.
;; Les vars exécutent la transformation dans un verrou.
;; Les agents implémentent la fonction exécution asynchrone.
;; Les messages envoyés à un agent sont mis en file d'attente pour être traités dans le futur, éventuellement
;; dans un autre thread.
;; Les messages sont traités de manière séquentielle, pas de double exécution possible
;; L'envoi d'un message est une opération non bloquante (vs vars)
;; Le comportement n'est pas porté par l'agent mais par les messages. L'agent ne fait que du contrôle, le
;; comportement vient de l'extérieur.
;; Les messages sont des fonctions de réduction, éventuellement accompagnées de valeurs.

;; A contrario, les réducteurs définissent uniquement du comportement et sont indépendants du contexte.
;; Un agent associé à un réducteur définit un comportement et un flux d'exécution, c'est donc une entité
;; autonome capable de réagir à des évènements.

(def ps (partial partial send))

(defn ! [f & args]
  (apply f args)
  f)


;; println, version thread-safe
(def ->out
  (ps (agent println) !))

(def ->err
  (ps (agent (fn [& args]
               (binding [*out* *err*]
                 (apply println args)))) !))

(comment
  (dotimes [n 10]
    (future (println (range n))))

  (dotimes [n 10]
    (future (->out (range n)))))



;; Avec transducers

(import '(java.time ZonedDateTime)
        '(java.time.format DateTimeFormatter))

(defn timestamp []
  (.format (ZonedDateTime/now) DateTimeFormatter/ISO_INSTANT))

(def levels (zipmap [:debug :info :warn :error :fatal] (range)))

(defn logger [log-level]
  (comp (map #(hash-map :level %1 :value %2))
        (filter #(<= (levels log-level) (levels (:level %))))
        (map #(str (timestamp) " " (name (:level %)) " " (:value %)))))

(def logger-dev (ps (agent ->out) ((logger :debug) !)))
(def logger-prod (ps (agent ->out) ((logger :warn) !)))

(comment
  (logger-dev :info "Something interesting happened.")
  (logger-dev :error "Something bad happened.")

  (logger-prod :debug "Just to say.")
  (logger-prod :warn "Something weird happened.")
  (logger-prod :fatal "Something very bad happened.")

  (logger-prod :important "gotcha !"))



(defn |
  ([output errors xf]
   (binding [*agent* (agent output
                            :validator #(if (reduced? %) (set-error-handler! *agent* nil) true)
                            :error-handler #(errors %2)
                            :error-mode :fail)]
     (ps *agent* (xf !))))
  ([output errors xf & xfs]
   (| (apply | output errors xfs) errors xf)))

(def |->log (partial | ->out ->err))

(def inv->log (|->log (map (partial / 1))))

(comment
  (inv->log 3)
  (inv->log 2)
  (inv->log 1)
  (inv->log 0))

(def sequential-4-adder
  (|->log (comp (map slow-inc)
                (map slow-inc)
                (map slow-inc)
                (map slow-inc)
                (map-indexed vector))))

(def pipelined-4-adder
  (|->log (map slow-inc)
          (map slow-inc)
          (map slow-inc)
          (map slow-inc)
          (map-indexed vector)))

(comment
  (reduce ! sequential-4-adder (range 10))
  (reduce ! pipelined-4-adder (range 10)))


;; Avec fonctions asynchrones

(defn after [t p & args]
  (future (Thread/sleep t) (apply p args)))

;; Ceci n'est pas un transducteur
(defn with-delay [t]
  (fn [rf]
    (let [out (ps *agent* rf)]
      (fn [r & args]
        (apply after t out args)
        r))))

(def delayed->out (|->log (with-delay 1000)))

(comment
  (delayed->out :plop))

(defn async [f]
  (fn [rf]
    (let [out (ps *agent* rf)
          err (ps *agent* #(throw %2))]
      (fn [r & args]
        (apply f out err args)
        r))))

(defn http-get [result error url]
  (h/get url {:async? true} result error))

(defn just [& args]
  (fn [rf]
    (apply send *agent* rf args)
    rf))

(comment
  (|->log (comp (just "https://clojure")
                (async http-get)))

  (|->log (comp (just "https://clojure.org")
                (async http-get)))

  (|->log (comp (just "https://clojure.org")
                (async http-get)
                (map (comp count :body))))
  )