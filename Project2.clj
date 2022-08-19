(ns project2.core)

(declare simplify)
(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false) (or x true)))
(def p3 '(or true a))
(defn and-simplify
  [andArgs]
  ;Returning a list without any true statements
  (let [noTrue (distinct (filter (fn [data] (not(true? data)))andArgs))]
    (cond
      ;Returning false if the list simplifies to false
      (some false? noTrue)false
      ;Returning true if the list simplifies to true
      (= (count noTrue) 1)true
      ;Returning the simplified list
      (= (count noTrue) 2) (first(rest noTrue))
      :else noTrue)))


(defn or-simplify
  [orArgs]
  ;Returning a list without any true statements
  (let [noFalse (distinct (filter (fn [data] (not(false? data)))orArgs))]
    (cond
      ;Returning false if the list simplifies to false
      (some true? noFalse)true
      ;Returning true if the list simplifies to true
      (= (count noFalse) 1)false
      ;Returning the simplified list
      (= (count noFalse) 2) (first(rest noFalse))
      :else noFalse)))

(defn not-simplify
  [notArgs]
  (cond
    ;Returning false if the list simplifies to false
    (some true? notArgs) false
    (some false? notArgs) true
    (and(seq? (last notArgs))(= 'not (first(last notArgs))))(last(last notArgs))
    (and(seq? (last notArgs))(= 'and (first(last notArgs))))(simplify(cons 'or(map #(list 'not %)(rest(last notArgs)))))
    (and(seq? (last notArgs))(= 'or (first(last notArgs))))(simplify(cons 'and (map #(list 'not %) (rest (last notArgs)))))
    :else notArgs
    )
  )


(defn search
  [i m]
  (get m i i))

(defn deep-substitute
  [l m]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i m)
           (search i m)))
       l))

(defn evalexp
  [exp bindings]
  (simplify (deep-substitute exp bindings)))

(defn simplify
  [arg1]
  (cond
    (seq? arg1)(let[simpleArgs (map simplify arg1)]
                 (cond
                   (=(first arg1) 'and)(and-simplify simpleArgs)
                   (=(first arg1) 'or)(or-simplify simpleArgs)
                   (=(first arg1) 'not)(not-simplify simpleArgs)))
    :else arg1))
