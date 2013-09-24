; http://norvig.com/spell-correct.html
;

; norvig python version of spell check.
; max score of {lang model, P(c) is english. and Error model, P(w|c) w means c }
;
; import re, collections
;
; def words(text): return re.findall('[a-z]+', text.lower()) 
;
; def train(features):
;     model = collections.defaultdict(lambda: 1)  # new word have default val 1.
;     for f in features:
;         model[f] += 1
;     return model
;
; NWORDS = train(words(file('big.txt').read()))
;
; alphabet = 'abcdefghijklmnopqrstuvwxyz'
;
; gen a set of all words that is 1 distance away [delete, transpose, replace, insert]
; def edits1(word):
;     [('', 'hello'), ('h', 'ello'), ('he', 'llo'), ('hel', 'lo'), ('hell', 'o'), ('hello', '')]
;    splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
;     ['ello', 'hllo', 'helo', 'helo', 'hell']
;     my version: [word[:i]+word[i+1:] for i in xrange(len(word))]
;    deletes    = [a + b[1:] for a, b in splits if b]
;     ['ehllo', 'hlelo', 'hello', 'helol']
;    transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
;    replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
;    inserts    = [a + c + b     for a, b in splits for c in alphabet]
;    return set(deletes + transposes + replaces + inserts)
;
; def known_edits2(word):
;     return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)
;
; def known(words): return set(w for w in words if w in NWORDS)
;
; def correct(word):
;     candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
;     return max(candidates, key=NWORDS.get)


(defn words [text] (re-seq #"[a-z]+" (.toLowerCase text)))
 
; put each word into hashmap with default cnt val to 1
(defn train [features]
  (reduce (fn [model f] (assoc model f (inc (get model f 1)))) {} features))
 
; entire word collection dict
(def *nwords* (train (words (slurp "big.txt"))))
 
; sequence comprehension to produce a set of all combinations of del, perm, repl, insert.
(defn edits1 [word]   ; given a word, mutate
  (let [alphabet "abcdefghijklmnopqrstuvwxyz" n (count word)]
    (distinct (concat
      ; seq comprehension, deletes one char a time
      (for [i (range n)] (str (subs word 0 i) (subs word (inc i))))
      ; transpose, swap one nb a time ("ehllo" "hlelo" "hello" "helol")
      (for [i (range (dec n))]  ; seq comprehension of all items
        (str (subs word 0 i) (nth word (inc i)) (nth word i) (subs word (+ 2 i))))
      ; seq thru, replace with seq of chars one at a time. 
      (for [i (range n) c alphabet] (str (subs word 0 i) c (subs word (inc i))))
      ; seq thru, insert from seq of chars one at a time.
      (for [i (range (inc n)) c alphabet] (str (subs word 0 i) c (subs word i)))))))
 
; ret a list of good english word from the passed in list of candidate words
(defn known 
  [words nwords]   ; take a  word list, seq thru validate each.
  (let [result (set (for [w words :when (nwords w)] w))]  ; [e for e in [0..8] when e % 2]
    (if (empty? result)
      nil
      result)))
 
; take a word, get dist 1 list, then for each dist 1 word, gen dist 2 word list
(defn known-edits2 
  [word nwords] 
  (set (for [e1 (edits1 word) e2 (edits1 e1) :when (nwords e2)]  e2)))

; when checking a word, first check if the word itself valid, then validate any of
; its dist-1, dist-2 word.  
(defn correct [word nwords]
  (let [candidates (or (known [word] nwords)  ; is word good english word ? 
                       (known (edits1 word) nwords) ; if not, dist 1 list of this word 
                       (known-edits2 word nwords)  ; then dist 2 list of this word 
                       [word])]  ; failed to find any of word, dist1, dist2 good, ret word itself
    ; among dist-1 2 mutation list, sel the max which (fn x) is the max
    (apply max-key #(get nwords % 1) candidates)))

; to use, check passed in against word dist collection
(correct "misstake" *nwords*)
(correct "speling" *nwords*)
