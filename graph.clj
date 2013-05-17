;;
;; basic graph. To run, (load-file "./graph.clj")
;; for full graph package, refer to github.com/jkk/loom
;; search graph in lisp is essentially list transform.
;; bfs/dfs is transform nb list into connectivity list by cons reachable nodes into the final lazy-seq.
;; recur until stack/q empty. recur binding or reduce to carry partial result collected 
;; for dijkistra, reduce on each min's nb list to update min heap, and recur until stack empty.
;;
(ns graph
    (:require clojure.string))

(defn dbg [msg]
  (prn msg))

;; Clojure has several ways of busting recursion
;;   explicit tail calls with recur. (they must be truely tail calls so this wont work)
;;   Lazy sequences as mentioned above.
;;   trampolining where you return a function that does the work instead of doing it directly 
;;   and then call a trampoline function that repeatedly calls its result until it turnes into a real value instead of a function.
;;   memoizing the the case of fact this can really shorten the stack depth, though it is not generally applicable.


;; lazy-seq : replace recursive with laziness.
;; wrap the recursive part of a function body with lazy-seq 
;; to replace recursion with laziness.
;; recursive part of fn body : (cons this_result (recursive-call (next iteration)))

;; lazy illustrated with fib seq
;; bad idea
(defn stack-consuming-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (stack-consuming-fibo (- n 1))
             (stack-consuming-fibo (- n 2)))))

;; lazy-seq
(defn fib
  ([idx pre cur]
    (let [next (+ pre cur)]
      (lazy-seq (cons next (fib (inc idx) cur next))))))

(defn lazy-seq-fibo
  ([]
    (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
    (let [n (+ a b)]
      (lazy-seq
        (cons n (lazy-seq-fibo b n))))))

;; The Fibonaccis are simply the first value of each pair.
;; iterate to gen a seq of pairs, use range to gen list of singles.
(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

;; memoize for bottom-up recursion.
;; you can see fib from inside memoized fn b/c fib is defed var.
;; if fib is a let binding, (let [fib (memoize (fn ...))]) you can not do this.
;; You need (with-local-vars to create thread-local var bindings and .bindRoot before leave with context.
;; or do Y-combinator to pass recursion fn in. (let [fib (fn [mem-fib x])])
(def fib (memoize (fn [x]
                    (if (< x 2)
                      x
                      (+ (fib (- x 1))
                         (fib (- x 2)))))))

;; true memoize fn that can recursively call itself.
;; inspired from http://stackoverflow.com/questions/3906831/how-do-i-generate-memoized-recursive-functions-in-clojure
(defmacro memoize-fn
  "Produces a memoized anonymous function that can recursively call itself."
  [fn-name & fn-args]
    `(with-local-vars
      [~fn-name (memoize (fn ~@fn-args))]
      (.bindRoot ~fn-name @~fn-name(def messages (ref ())))
    @~fn-name))

;;
;; fib defed by co-recursion 
;; recursion: break from top down to smaller bottom 
;; co-recursion: use data gened by fn, bit by bit, to produce further bits of data.
;; so re-cursively def fib as [0 1 f]
;; fib                0        1      f
;; (rest fib)         1        f
;; (+ fib (rest fib)) f

(def fib (lazy-cat [0 1] (map + fib (rest fib))))

;; factorial with recur
(defn fact [x]
  (loop [n x f 1]
    (if (= n 1)
      f
      (recur (dec n) (* f n)))))

;; lazy-seq version
(defn factorials []
  (letfn [(factorial-seq [n fact]
            (lazy-seq
              (cons fact (factorial-seq (inc n) (* (inc n) fact)))))]
    (factorial-seq 1 1)))

;; factorial is just reduce of a list of consecutive num.
(defn factorial [n]
  (reduce * (range 1 (inc n))))

;; call trampolining to repeatively call your fn that does the factorial.   
;; trampolining where you return a function that does the work instead of doing it directly 
;;   and then call a trampoline function that repeatedly calls its result until it turnes into a real value instead of a function.
(defn fact [x]
  (if (<= x 1)
    1
    #(* x (fact (- x 1)) )) )   ;; ret a fn instead of calc the factorial directly.

(trampoline (fact 42))   ;; ask trampoline to repeatively call the fn.


;;
;; run-time cause PersistentQueue's multimethod dispatching to use this version of print-method.
;;
(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

;; first, convert a set of edges to a nb map.
;; use update-in to conj nbs into the key.
(defn nbmap [col]
  (let [ edges (concat col (map (fn [e]
                    [(last e) (first e)]) col))]
    (reduce (fn [ret e]
              (update-in ret [(first e)] (fnil conj #{}) (last e)))
            {} edges)))

;; argument col is a set of edges [a b], translate to nb map.
;; color set contains discovered node. do not enqueue discovered node.
;;
(defn bfs [col]
  (letfn [(nbmap [col]      ;; transform edge list into neighbor adjacent nodes map
            (let [edges (concat col (map (fn [e] [(last e) (first e)]) col))]
              (reduce (fn [ret e] (update-in ret [(first e)] (fnil conj #{}) (last e))) {} edges)))

          ;; Q is persistentQueue
          (stepHd [graph q color]       ;; deq Q head and process each node
            (when-let [hd (peek q)]     ;; while q is not empty, reduce on the neighbor of header
              ;; process node early, process each edge, then process node late.
              (lazy-seq        ;; stepHd fn rets a lazy seq of all reachable nodes
                (cons hd           ;; bfs, all nodes in Q are reachable, add this round reachable to partial result
                  (let [hdnb (remove (fn [e] (contains? color e)) (graph hd))]  ;; at leaf level, an empty lazy-seq
                    ;;(prn "hd " hd " hdnb " hdnb " color " color q)
                    ;; recur call - pop head, add head's neighbor to queue, add head's neighbor to colormap(in stack visiting)
                    (stepHd graph (into (pop q) hdnb) (reduce #(conj %1 %2) color hdnb)))))))]   ;; reduce to translate hdnb list to atom for conj

    (let [graph (nbmap col)             ;; transform edge list into neighbor map
          root (first (keys graph))]
      (= (count graph) (count (stepHd graph (conj clojure.lang.PersistentQueue/EMPTY root) (conj #{} root)))))))

(defn graph-tour [ edges ]
  (letfn [(nbmap [col]  
                 ;; collection of edges [[a b] [c d]]
                 (let [bi-edges (into col (map (fn [e] [(second e) (first e)]) col))]
                   (reduce (fn [ret e]
                             (update-in ret [(first e)] (fnil conj #{}) (last e))) {} bi-edges)))

          (bfs [nbmap q discovered]  
                  ;; q is PersistentQueue, conj to discovered set when we find a new node, until q is empty.
                  (let [hd (peek q)   ;;  
                        hdnb (remove #(contains? discovered %) (nbmap hd))]
                    (if (empty? hdnb)
                      discovered
                      (recur nbmap (into (pop q) hdnb) (reduce #(conj %1 %2) discovered hdnb)))))
          (stepHd [nbmap q discovered] 
                  ; stepHd process head and ret a lazy list of node reachable from head.
                  (when-let [hd (peek q)]
                    (lazy-seq
                      (cons hd
                            (let [hdnb (remove #(contains? discovered %) (nbmap hd))]
                              (stepHd nbmap (into (pop q) hdnb) (reduce #(conj %1 %2) discovered hdnb)))))))
          (odd-degrees [edges]
                      ;; ret the num of nodes that have odd degrees
                      (let [out-deg-map (reduce (fn [ret e] (update-in ret [(first e)] (fnil inc 0))) {} edges)
                            in-deg-map (reduce (fn [ret e] (update-in ret [(second e)] (fnil inc 0))) {} edges)
                            deg-map (merge-with + out-deg-map in-deg-map)
                            odd-deg (filter (fn [e] (odd? (val e))) deg-map)]
                        (count odd-deg)))]
    (let [graph (nbmap edges)
          root (key (first graph))
          q (conj clojure.lang.PersistentQueue/EMPTY root)
          discovered (conj #{} root) 
          stephdcomponent (stepHd graph q discovered)
          bfscomponent (bfs graph q discovered)]
      (prn graph)
      (prn (take 5 (stepHd graph q discovered)))
      (prn bfscomponent))))

(graph-tour [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]])

;;
;; DFS. We need to transform fn recur to loop recur, as we need to recur bind/carry intermeidate result from each
;; fn recur branch. In oo, ret val and call stack handles it.
;;    largest = max( (for c in child) val = dfs(c))
;; In fn lang, need to loop with explicit stack until stack empty and bind carry intermediate result.
;; dfs traverse normally post-order, as we want to collect all results from children's explore.
;; Vectors and Lists used as stacks with IPersistenStack to bind Intermediate result
;; partial result can be a vec of visited node, or a map of max/min of each node's subtree.
;; discovered : node in active stack. was set when node gets put into stack. 
;; processed : node all children done. Not in active stack. Only used in cyclic graph. Not usable in b-tree.
;; we update state and collect result for each node after all its children done.
;; we can carry a map to record each node's max/min after exploring all node's children.
;;
(defn dfs [col]
  (let [graph (nbmap col)    ;; tranform edge list to neighbor adj map.
        root (first (keys graph))]
    (loop [graph graph stack [root] discovered #{root} processed #{} partRslt []]   ;; stack bottom is root when started.
      (if (empty? stack)       ; recur until stack empty
        [partRslt discovered]  ; loop ret partial result. recursive fn call ret []
        (let [ curnode (peek stack)    ; peek stack, not pop, find all children of the cur node.
               children (remove (fn [e] (contains? discovered e)) (graph curnode))
               child (first children)]
          (if child   ; if children not done, keep pushing stack topnode's children. Otherwise, pop topnode from stack.
            (recur graph (conj stack child) (conj discovered child) processed (conj partRslt [curnode child]))  ;; path is from curnode to child
            (recur graph (pop stack) discovered (conj processed curnode) partRslt )))))))

; fn build the full solution on top of the paritial solution recursion.
; the key is found partial result, and recursively build full solution on top it.
; dfs ret a lazy seq of node from dfs of root, always only look at the header
; how to perf process-edge, process-vertex late ?
(defn dfs-tree [root]  ; for bin tree, acyclic, no need for discovered/processed
  (letfn [(get-children [root]
            ; give a root, reduce(loop) of all its children(left,right) to collect
            (reduce (fn [ret cur] (conj ret (cur root))) [] [:left :right]))]
    (let [children (filter identity (get-children root))]
      (if (empty? children)
        [(:val root)]  ; at end, ret single node val. at parent, use reduce to culmulate partial result.
        (let [partRslt (reduce (fn [ret cur] (lazy-cat ret (dfs-tree cur))) [] children)]
          (lazy-cat partRslt [(:val root)]))))))  ; append root at the end of children rets.

(dfs-tree my-tree)

; using mapcat to replace reduce a list to another list
(defn dfs-tree-mapcat [root]
  (letfn [(get-children [root]
            ;(filter identity (reduce (fn [ret cur] (conj ret (cur root))) [] [:left :right])))]
            (filter identity (mapcat #(vector (% root)) [:left :right])))]
    (if (empty? root)
      [(:val root)]   ; base, ret root val
      (conj (vec (mapcat #(dfs-tree-mapcat %) (get-children root))) (:val root)))))
   
(dfs-tree-mapcat my-tree)

; dfs of graph, need to carry global discovered, processed, and path
; The args to recursive fn can carry global state; foreach c : children, partRslt = dfs(c)
; we reduce(cumulate result of each children, update to the global state, and carry it over
; to the next dfs(child) call. After all dfs children done, we have full dfs result for this node.
; the same as DP call that carries partial result at each DP. e.g, subset-sum.
(defn dfs-graph [root discovered processed path]
  (letfn [(get-children [root]
          ; collect all children of root in a vec to operate on
          (reduce (fn [ret cur] (conj ret (cur root))) [] [:left :right]))]
    (let [children (filter identity (get-children root))]
      ; reduce can only take one state var. use loop recur to carry multiple
      (loop [children children discovered discovered processed processed path path]
        (if (empty? children)  
          ; at end, ret a vec of updated global state.(processed, path)
          [discovered (conj processed root) (lazy-cat path [(:val root)])]
          (let [ hd (first children) 
                 discovered? (some #{hd} discovered)
                 upd-disc (conj discovered hd) ]
            ; only recursion(node, edge) on not discovered. do edge on processed.
            (if-not discovered?
              ; dfs recur fn with carried global 
              (let [[accu-disc accu-proc accu-path] (dfs-graph hd upd-disc processed path)]
                ; accumulate one child's partial result into global state for recur to carry on.
                (recur (rest children) accu-disc accu-proc accu-path))
              ; node already disc, process edge if applicable. otherwise, skip
              (recur (rest children) upd-disc processed path))))))))

(dfs-graph my-tree #{} #{} [])


;;
;; bfs with co-recursion. This is space search algorithm. 
;; bfs needs a queue rather than recursion. With co-recursion, queue is implicit by lazy-cons head to the result.
;; From head, recursively search out the entire space.
;; use the data gened by fn, produce further data.
;; use the available data, head, produce further data, header's children.
;; ret a seq formed by cons hd into the seq formed by further data from processing of head.
;;
(defstruct tree :val :left :right)
(def my-tree
  (struct tree 1
    (struct tree 2)
    (struct tree 3
      (struct tree 4
        (struct tree 66))
      (struct tree 5))))

; arg is a list of node, of one bfs level. process the arg, produce a result list which consists
; of all of children of nodes in cur bfs level. this result list is the arg for next recursive bfs.
; the final result is the cons of cur list on top of 
(defn bftrav [& trees]  ; trees is a Q in bfs
  (when trees
    (lazy-cat trees     ; cons me to the result of processing me, forming the full result.
      (->> trees
        (mapcat #(vector (:left %) (:right %)))  ; process cur list, the result is the input for next recursion
        (filter identity)    ;; filter identity remov nil.
        (apply bftrav)))))

(bftrav my-tree)


; when co-recursion, the diff between bfs and dfs, is when lazy-cat parent result to child result.
; recursion is apply DP fn to header, and recursively apply DP fn to all header's children. then
; concat the result. 
;   (lazy-cat (hd) (mapcat DP (getChildren hd)))  ; bfs cons hd
;   (lazy-cat (mapcat DP (getChildren hd)) (hd))  ; dfs conj hd
; For tree, get child of head is header's child pointer
; For list, get child of head is header's idx dec. List is a tree with only left children.
; we unified graph traverse branch bound explore with list header recursion 
; example:
;  permutation, get hd, header's child is head idx-1, permu(childen), for each result, conj hd.
;  subsetsum: get hd, child is head idx-1, apply subsetsum to children incl/excl head value.
;  dfs : lazy-cat conj result of dfs header's children, with header
;  bfs : mapcat getchild fn to each headers, apply bfs to all the children of cur headers.
;        lazy-cat headers to the results of recursive bfs to the current headers.
;
(defn dftrav [& trees]
  (when trees
    (lazy-cat 
      (->> trees
        (mapcat #(vector (:left %) (:right %)))
        (filter identity)
        (apply dftrav))
      trees)))

(dftrav my-tree)


;; for binary tree, in order traverse just recursively cons lchild, cur, rchild.
(defn xseq [tree]
  (when tree
    (concat (xseq (:l tree)) [(:val tree)] (xseq (:r t))))) 


;; take a nest collection, and a sub collection of it that sum to certain number. maintain nested structure.
;; For/loop comprehents flatten list. Nested collection, need explict loop or reduce and carry partial result along.
;; for list comprehen
;;
(fn SequsHorribilis
  ([tot xs]
    (sequs tot xs []))

    ([tot xs partResult]     ;; xs must be a seq when calling.
      (loop [ remain tot
              xs xs
              partResult partResult]
        (if (empty? xs)      ;; break out loop when empty list
          partResult

          (let [ hd (first xs)
                 body (rest xs)
                 t (type hd) ]
            (if (or (= t clojure.lang.PersistentVector)
                    (= t clojure.lang.PersistentList))
              ;;
              ;; if header is a collection, call this fn recursively to get result for header,
              ;; and continue loop the rest of the list with the result from head conjed to partial result.
              ;;
              (let [headrslt (sequs remain hd [])  ;; call myself to get result for head collection.
                    headtot (apply + (flatten headrslt))]
                (recur (- remain headtot) body (conj partResult headrslt)))  ;; loop the rest with head's result conjed to partial result.
              (if (>= remain hd)
                (recur (- remain hd) body (conj partResult hd))
                partResult)))))))

(=  (__ 10 [1 2 [3 [4 5] 6] 7]) '(1 2 (3 (4)))')
(=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11]) '(1 2 (3 (4 (5 (6 (7))))))')
(=  (__ 9 (range)) '(0 1 2 3)')
(=  (__ 1 [[[[[1]]]]]) '(((((1)))))')
(=  (__ 0 [1 2 [3 [4 5] 6] 7]) '()')
(=  (__ 0 [0 0 [0 [0]]]) '(0 0 (0 (0)))')
(=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]]) '(-10 (1 (2 3 (4))))')


;;
;; dijkstra short-path
;; ret a lazy-seq of of [curnode {state}] where state map {node [dist parent]}
;; (graph u) ret a vec of nb for u. (dist u v) ret the dist between u-v
;; alg: each node's dist to src is in a priq list. transform the list one step at the hd, relaxed create a new list. Recur until list empty.
;; In impl, the priq dist is a sorted-map by value with key is node and val is the dist to src node. Init it first.
;; each recur, head is min. pop head, update priq for head's each child, if src-child thru head is closer.
;;
;;
(defn dijkstra [graph dist start]
  (let [vertice (remove #(= % start) (keys graph))
        priq (reduce merge (map #(hash-map % (Integer/MAX_VALUE)) {start 0}) vertice)]   ;; init dist[src,v]
    (loop [partpriq priq partRslt {}]
      (if (empty? partpriq)   ;; done looping over all vertices
        partRslt
        (let [[hd [dist-hd hd-parent]] (first partpriq)   ;; map entry is {node [dist parent]}
              hdchildren (graph hd)
              rmhdpriq (reduce merge {} (-> partpriq vec rest))  ;; remove head from queue.
              relaxedmap (reduce (fn [ret hd-child]   ;; relax priq with head's nbs
                           (let [dist-srchdchild (+ dist-hd (dist hd hd-child))
                                 dist-srcchild (first (rmhdpriq hd-child))]      ;; current s-v dist
                             (if (< dist-srchdchild dist-srcchild)        ;; relax if new route smaller
                               (assoc ret hd-child (vector dist-srchdchild hd))  ;; update result map, kin reduce.
                               ret)))  ;; dist not smaller, no relax, ret the old value.
                           rmhdpriq hdchildren)   ;; reduce head's children list.
              ]
          (recur
            (into (sorted-map-by (fn [k1 k2] (compare [(get relaxedmap k1) k1] [(get relaxedmap k2) k2])))  ;; sort priq map by dist value.
                  relaxedmap)
            (conj partRslt (partpriq))))))))


(prn "bfs 1 " (= true (bfs #{[:a :a]})))

(prn "bfs 2 " (= true (bfs #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]})))

(prn "bfs 3 " (= false (bfs #{[:a :b] [:b :c] [:c :d]
               [:x :y] [:d :a] [:b :e]})))

(prn "dfs 1 " (dfs #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e]}))

(prn "dfs 2 " (dfs #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]}))


;; leveinshtein edit distance
;; use memoize to for dynamic programming.
;;
(def dist (memoize (fn [pred index]
  (let [[i j] index]
    (cond
      (zero? (min i j)) (max i j)
      (pred index) (dist pred [(dec i) (dec j)])
      :else (inc (min
                   (dist pred [(dec i) j])
                   (dist pred [i (dec j)])
                   (dist pred [(dec i) (dec j)]))))))))

(defn levenshtein-distance
  (let [pred (fn [index]
                (let [[i j] index]
                  (= (get source (dec i)) (get target (dec j)))))]
    (->> (for [j (range (count target))
               i (range (count source))]
           [i j])
      (map (partial dist pred))
      last)))
