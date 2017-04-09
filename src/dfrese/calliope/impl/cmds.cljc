(ns dfrese.calliope.impl.cmds
  (:require [dfrese.calliope.ext :as ext]
            [dfrese.clj.functions :as f]))

(defrecord TransformedCmd [base t]
  ext/ICmd
  (-run! [this context]
    (ext/-run! base (ext/update-dispatcher context #(f/comp % t)))))

(defrecord BatchedCmds [cmds]
  ext/ICmd
  (-run! [this context]
    (doseq [c cmds]
      (ext/-run! c context))))

(defn batch-cmds [cmd & cmds]
  (BatchedCmds. (remove nil? (cons cmd cmds))))

(defn translate-cmd [cmd t & ts]
  (cond
    (instance? BatchedCmds cmd)
    (BatchedCmds. (map #(apply translate-cmd % t ts)
                       (.-cmds ^BatchedCmds cmd)))
    
    (instance? TransformedCmd cmd)
    (TransformedCmd. (.-base ^TransformedCmd cmd)
                     (apply f/comp (concat ts [t (.-t ^TransformedCmd cmd)])))
    
    (satisfies? ext/ICmd cmd)
    (TransformedCmd. cmd (apply f/comp (concat ts [t])))
    
    :else
    (assert false (str "Not a command:" cmd))))

