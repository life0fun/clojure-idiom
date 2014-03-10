;;
;; to use core.async, need to add core.async dependency.
; core.async is a lib, which consists of macros that rewrite
; your code into CSP style. As it is a lib, just dependent on it.
; 
; (defproject com.colorcloud/parallel "0.1.0-SNAPSHOT"
;   :dependencies [[org.clojure/clojure "1.5.1"]
;                  [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]])



(ns com.colorcloud.parallel
  (:require [clojure.core.async :refer [go go-loop <! >! alts! close!]]))

(defn parallel
  "Processes values from input channel in parallel on n 'go' blocks.

  Invokes f on values taken from input channel. Values returned from f
  are written on output channel.

  Returns a channel which will be closed when the input channel is
  closed and all operations have completed.

  Note: the order of outputs may not match the order of inputs."
  [n fnc input output]
  (let [tasks (doall
               (repeatedly n
                #(go-loop []
                   (let [in (<! input)]
                     (when-not (nil? in)
                       (let [out (fnc in)]
                         (when-not (nil? out)
                           (>! output out))
                         (recur)))))))]
    (go (doseq [task tasks]
          (<! task)))))


; process a vector of chan endpoints with maximum number of go blocks or threads.
; the function that process the value must ret a chan, where the result is stored.
; alts! will multi-wait on a vector of chan endpoints.
;
(defn pmax
  "Process messages from input in parallel with at most max concurrent
  operations.

  Invokes f on values taken from input channel. f must return a
  channel, whose first value (if not closed) will be put on the output
  channel.

  Returns a channel which will be closed when the input channel is
  closed and all operations have completed.

  Creates new operations lazily: if processing can keep up with input,
  the number of parallel operations may be less than max.

  Note: the order of outputs may not match the order of inputs."
  [max fnc ichan ochan]
  (go-loop [tasks #{ichan}]  ; go-loop on a vector of chan endpoints
    (when (seq tasks)   ; nil pun with seq
      (let [[value task] (alts! (vec tasks))] ; multi-wait on a vector of chan endpoints
        (if (= task ichan)
          (if (nil? value)
            (recur (disj tasks task))  ; ichan is closed
            (recur (conj (if (= max (count tasks))  ; max - 1 tasks running
                           (disj tasks ichan)  ; temporarily stop reading ichan
                           tasks)
                         (fnc value))))  ; fnc must ret a chan, where result is stored
          ;; one processing task finished: continue reading ichan
          (do (when-not (nil? value) (>! ochan value))
              (recur (-> tasks (disj task) (conj ichan)))))))))



; drain the chan and put the value to sink []
(defn sink
  "Returns an atom containing a vector. Consumes values from channel
  ch and conj's them into the atom."
  [ch]
  (let [sink (atom [])]   ; ensure atomic update of [] using a fn
    (go-loop []
      (let [val (<! ch)]
        (when-not (nil? val)
          (swap! sink conj val)  ; atomically conj
          (recur))))
    sink))


; add a watch fn to an agent/atom/ref/var reference. 
; The watch fn 4 args: a key, the reference, its old-state, new-state.
; counter is a mutable int count refed by atom. 
; Keys must be unique per reference, and can be used to remove the watch with 
; remove-watch, but are otherwise considered opaque bythe watch mechanism
(defn watch-counter 
  "watch atom counter, get it value, throw into thread-counts list"
  [counter thread-counts]
  (add-watch counter
             :thread-count
             (fn [_ _ _ thread-count]
               (swap! thread-counts conj thread-count))))


(deftest t-parallel
  (let [input (to-chan (range 50))
        output (chan)
        result (sink output)
        max-threads 5
        counter (atom 0)
        f (fn [x]
            (swap! counter inc)
            (Thread/sleep (rand-int 100))
            (swap! counter dec)
            x)
        thread-counts (atom [])]
    (watch-counter counter thread-counts)
    (<!! (parallel max-threads f input output))
    ;; First, we want to make sure that the input matches the output,
    ;; but we can't assume they are in the same order, so we convert
    ;; to sets.
    (is (= (set (range 50)) (set @result)))
    ;; Then, we want to make sure that the thread count never got
    ;; above our stipulated maximum.
    (is (every? #(<= % max-threads) @thread-counts))))

(deftest t-pmax-go
  (let [input (to-chan (range 50))
        output (chan)
        result (sink output)
        max-threads 5
        counter (atom 0)
        fnc ; fnc must within go block and ret a chan
          (fn [x]
            (go  ; go async exec the body, go block is a chan, can use thread
             (swap! counter inc)
             (<! (timeout (rand-int 100)))
             (swap! counter dec)
             x))
        thread-counts (atom [])]
    (watch-counter counter thread-counts)
    (<!! (pmax max-threads fnc input output))
    (is (= (set (range 50)) (set @result)))
    (is (every? #(<= % max-threads) @thread-counts))))


(deftest t-pmax-slow-input
  (let [input (chan)
        output (chan)
        result (sink output)
        max-threads 5
        actual-needed-threads 3
        counter (atom 0)
        f (fn [x]
            (go
             (swap! counter inc)
             (<! (timeout (rand-int 100)))
             (swap! counter dec)
             x))
        thread-counts (atom [])]
    (watch-counter counter thread-counts)
    ;; Slow input:
    (go-loop [i 0]
      (if (< i 50)
        (do (<! (timeout 50))
            (>! input i)
            (recur (inc i)))
        (close! input)))
    (<!! (pmax max-threads f input output))
    (is (= (set (range 50)) (set @result)))
    (is (every? #(<= % actual-needed-threads) @thread-counts))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; core.async dot games
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns dots-game
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! alts! timeout]]
   [jayq.core :refer [$ append ajax inner css $deferred
                      when done resolve pipe on bind attr
                      offset] :as jq]
   [jayq.util :refer [log]]
   [crate.core :as crate])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]))

; gestures are a composition of events.
(defn xy-message [ch msg-name xy-obj]
  (put! ch [msg-name {:x (.-pageX xy-obj) :y (.-pageY xy-obj)}]))

(defn touch-xy-message [ch msg-name xy-obj]
  (xy-message ch msg-name
              (aget (.-touches (.-originalEvent xy-obj)) 0)))

; when mouse moving, put :draw event into chan. otherwise, put :drawend to chan.
(defn mousemove-handler [in-chan jqevent]
  (if (pos? (.-which jqevent))
    (xy-message in-chan :draw jqevent)
    (put! in-chan [:drawend])))

; capture gesture UI events, and put them into chan.
(defn draw-event-capture [in-chan selector]
  (let [end-handler (fn [_] (put! in-chan [:drawend]))]
    (bind ($ selector) "mousemove" #(mousemove-handler in-chan %))
    (bind ($ selector) "mousedown" #(xy-message in-chan :draw %))
    (bind ($ selector) "mouseup"   end-handler)
    (bind ($ selector) "touchmove" #(touch-xy-message in-chan :draw %))
    (bind ($ selector) "touchend"  end-handler)))

; emits a stream of :draw evt from ichan to ochan.
; get-drawing will only emit :draw messages until it receives a message that isn’t a :draw message
(defn get-drawing [input-chan out-chan]
  "continue process :draw event in input-chan, and put to out-chan"
  (go-loop [msg (<! input-chan)]
    (put! out-chan msg)
    (when (= (first msg) :draw)
      (recur (<! input-chan)))))

; given a selector, ret a chan that emits drawing evts in the div.
(defn draw-chan [selector]
  (let [input-chan (chan)
        out-chan   (chan)]
    ; first, get all UI events into chan
    (draw-event-capture input-chan selector)
    ; go-loop chan events, filter :draw event to out-chan
    (go-loop [[msg-name _ :as msg] (<! input-chan)]
      (when (= msg-name :draw)
        (put! out-chan msg)
        (<! (get-drawing input-chan out-chan)))  ; drain ichan draw evts to ochan.
      (recur (<! input-chan))))
    out-chan)

(defn draw-point [selector color coord {:keys [top left]}]
  (append ($ selector)
          (crate/html [:div {:class (str "point " (name color))
                             :style (str "top: " (- (coord :y) top 5) "px;"
                                         "left: " (- (coord :x) left 5) "px;")}])))
; consume evts from drawing chan, put point in div.
(defn draw-points [selector drawing-chan color]
  (let [offset   (offset ($ selector))
        width    (+ (.width ($ selector)) (offset :left))
        height   (+ (.height ($ selector)) (offset :top))
        in-range #(and (< (offset :top) (% :y) height)
                       (< (offset :left) (% :x) width))]
    (go-loop [[msg-name xy-obj] (<! drawing-chan)]
      (when (= msg-name :draw)
        (if (in-range xy-obj)
            (draw-point selector color xy-obj offset))
        (recur (<! drawing-chan)))))))

; create drawing chan from div.
(defn drawing-loop [selector]
  (let [drawing-chan (draw-chan selector)]
    (go-loop [[msg-name msg-data] (<! drawing-chan) 
              color-i 0]
      (if (= :draw msg-name)
        ; starting drawing upon :draw evt until :drawend.
        (<! (draw-points selector
                         drawing-chan
                         (get [:red :green :blue] (mod color-i 3)))))
       (recur (<! drawing-chan) (inc color-i))))))


;;;;
; helper functions to render a board of random colored dots
;;;;
; create a div to contain a dot at pos i with color.
(defn dot-templ [i color]
  (let [[top left] (dot-pos-to-corner-position i)
        class (str "dot " (name color))
        style (str "top:" top "px; left: " left "px;")]
    [:div {:class class :style style}]))

(defn create-dot [i color]
  {:color color :pos i :elem (crate/html (dot-templ i color))})

(defn render-state [selector board]
  (mapv #(append ($ selector) (:elem %)) board))

(defn render-dots [selector]
  (render-state selector
                (map-indexed create-dot (get-rand-colors board-size)

;;;
; map drawing gesture evt into dot position
;;;

(def reverse-board-position (partial - (dec board-size)))

; from draw evt coord to dot pos
(defn coord->dot-pos 
  [offset {:keys [x y]}]
  (let [[x y] (map - [x y] offset [13 13])]
    (when (and (< 12 x (+ 12 grid-unit))
               (< 12 y (* board-size grid-unit)))
      (reverse-board-position (int (/ y grid-unit))))))

(defn collect-dots 
  [draw-input out-chan board-offset init-msg]
  (go-loop [last-pos nil
            msg init-msg]
    (when (= :draw (first msg))
      (let [cur-pos (coord->dot-pos board-offset (last msg))]
        (if (and (not (nil? cur-pos)) (not= cur-pos last-pos))
            (put! out-chan [:dot-pos cur-pos]))
        (recur (or cur-pos last-pos) (<! draw-input)))))))

; dot-chan on top of draw-chan, to a stream of dot pos.
(defn dot-chan [selector]
  (let [draw-input (draw-chan selector)
        board-offset ((juxt :left :top) (offset ($ selector)))
        out-chan (chan)
        dot-collector (partial collect-dots 
                               draw-input out-chan board-offset)]
    ; div evt in draw chan to dots in div
    (go-loop [msg (<! draw-input)]
      (when (= (first msg) :draw)
        (<! (dot-collector msg))
        (put! out-chan [:end-dots]))
      (recur (<! draw-input)))
    out-chan))


; decoupling evt src and main app code. do not do too much in evt handle with actions.
; Let evt handler to insert msg into evt queue.

(def create-dots #(map-indexed create-dot (get-rand-colors %)))

(defn add-dots-to-board [selector dots]
  (mapv #(append ($ selector) (:elem %)) dots))

; user mousemove creats dot chain, a vec of [:dot-pos 2 :dot-pos 3 :end-dots]
(defn get-dot-chain 
  [state dot-ch first-dot-msg]
  (go-loop [dot-chain []   ; dot chain is a vec of dot that selected by user
            msg first-dot-msg]
    (if (not= :dot-pos (first msg))  ; append :dot-pos to dot chain, or :end-dots done.
      dot-chain
      (recur (conj dot-chain (last msg)) (<! dot-ch)))))) 

; :dot-chain is a key of state map
(defn dot-chain-getter
  [state dot-ch]
  (go-loop [dot-msg (<! dot-ch)]  ; read evt from dot-ch, if its :dot-pos, get dot-chain
     (if (= :dot-pos (first dot-msg))
       (<! (get-dot-chain state dot-ch dot-msg))
       (recur (<! dot-ch))))))

; div has dot-chan(draw-chan), state has :dot-chain.
; block wait for dot-chain-getter to ret a dot-chain and render the dot-chain
(defn game-loop [selector init-state]
  (let [dot-ch (dot-chan selector)]
    (add-dots-to-board selector (init-state :board))
    (go-loop [state init-state]
      (let [state (assoc state :dot-chain (<! (dot-chain-getter state dot-ch)))]
         (recur (render-updates state)))))))

(defn render-updates [state]
  (if (pos? (count (state :dot-chain)))
    (remove-dots state)
    state))

; remov user selected dots in the dot-chain.
(defn remove-dots [{:keys [dot-chain] :as state}]
  (let [pos-set        (set dot-chain)
        dots-to-remove (keep-indexed #(if (pos-set %1) %2) (state :board))
        next-board     (keep-indexed #(if (not (pos-set %1)) %2) 
                                          (state :board))]
    (remove-dots-from-dom dots-to-remove)
    (move-dots-to-new-positions next-board)
    (assoc state :board (update-positions next-board) :dot-chain [])))

; doseq all dots, in parallel, launch many go blocks to remove dots.
(defn remove-dots-from-dom [dots-to-remove]
  (doseq [dot dots-to-remove]
    (go
     (let [$elem ($ (dot :elem))]
       (.addClass $elem "scale-out")
       (<! (timeout 150))  ; animation, wait for 150.
       (.remove $elem)))))

(defn add-dots-to-board [selector dots]
  (mapv #(append ($ selector) (:elem %)) dots))

(defn move-dot-to-pos [dot i]
  (let [[top left] (dot-pos-to-corner-position i)]
    (css ($ (dot :elem)) {:top top :left left})))

; while pos++, if dot.pos not equal current pos, mov dot to fill cur pos.
(defn move-dots-to-new-positions [board]
  (go-loop [i 0 [dot & xdots] board]  ; pos [0 1 2 3], dots [d1 nil nil d4]
     (when (not (nil? dot))
       (when (not= (dot :pos) i)
         (move-dot-to-pos dot i)
         (<! (timeout 100)))
       (recur (inc i) xdots))))

(defn update-positions [board]
  (vec (map-indexed #(assoc %2 :pos %1) board)))


; add new dot at off screen pos, and re-use animation in move dots to new pos
(defn add-dots [state]
  (let [number-to-add (- board-size (count (state :board)))
        new-dots (map create-dot (repeat 8) (get-rand-colors number-to-add))
        next-board (concat (state :board) new-dots)]
    (add-dots-to-board (state :selector) new-dots)
    (go
     (<! (timeout 500))
     (move-dots-to-new-positions next-board))
    (assoc state :board (update-positions next-board))))

