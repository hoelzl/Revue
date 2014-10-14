;;; Work in progress that didn't make it into the code base, yet.

;;; Global Environments
;;; ===================

;;; We need to be able to easily separate functions defined by the
;;; user from those defined by the system or by libraries, and we need
;;; to do so rather frequently.  So rather than filtering a flat
;;; global environment for every step of the trace we define a type
;;; `GlobalEnv` that contains two maps: `system-env` and `user-env`.
;;; Lookups generally take place in both maps, but we are only
;;; interested in user-defined function (which we usually are for
;;; visualization purposes), we can easily detach the user
;;; environment.

(defprotocol IGlobalEnv
  "The protocol for global environments: return the system and the
  user environment."
  (system-env [this])
  (user-env [this]))

(declare ->GlobalEnv)
(def ^:dynamic *defining-system-env*
  "If truthy, make global changes to `GlobalEnv` instances in their
  system environment, otherwise change their user environment."
  false)

;;; Implementing Map-Like Datatypes
;;; -----------------------------

;;; Since the implementation of datatypes that work like maps and
;;; sequences is, as far as I know, not well documented in the Clojure
;;; literature, I've included an overview of the class hierarchy.  In
;;; Clojure, the necessary interfaces are defined in Java and can be
;;; found in the `clojure.lang` namespace, in the directory
;;; `clojure/src/jvm/clojure/lang`.  The "protocols" are implemented
;;; as Java interfaces, so they are not available as protocols for
;;; Clojure code outside the `deftype` macro.  So you can't evaluate
;;; either of
;;;
;;;     (satisfies? Counted {})              ;; BAD
;;;     (satisfies? clojure.lang.Counted {}) ;; BAD
;;;     (satisfies? clojure.lang/Counted {}) ;; BAD
;;;
;;; to check whether something implements the `Counted` protocol, since
;;; all three forms will throw an exception for any Clojure object.

;;; The abstract implementation of a map is `APersistentMap` which
;;; extends the class `AFn` and implements the interfaces
;;; `IPersistentMap`, `Map`, `Iterable`, `Serializable`,
;;; `MapEquivalence` and `IHashEq`.  The following table gives an
;;; overview of the inheritance hierarchy and methods implemented:

;;; ```
;;; AFn:                   IFn
;;; IFn:                   Callable, Runnable
;;;   invoke
;;; IPersistentMap:        Iterable, Associative, Counted
;;;   assoc
;;;   assocEx
;;;   without
;;; Iterable:
;;;   iterator
;;; Associative:           IPersistentCollection, ILookup
;;;   containsKey
;;;   entryAt
;;;   assoc
;;; IPersistentCollection: Seqable
;;;   count
;;;   cons
;;;   empty
;;;   equiv
;;; ILookup:
;;;   valAt
;;; Seqable:
;;;   seq
;;; Counted:
;;;   count
;;; Map: (required methods only)
;;;   containsKey
;;;   containsValue
;;;   entrySet
;;;   equals
;;;   get
;;;   hashCode
;;;   isEmpty
;;;   keySet
;;;   size
;;;   values
;;; Serializable:
;;;   writeObject
;;;   readObject
;;;   readObjectNoData
;;; MapEquivalence:
;;; IHashEq
;;;   hasheq
;;;```

;;; The handling of equality and hashes is a bit complicated because
;;; it relies on implementation internals.
;;; See [these](http://dev.clojure.org/jira/browse/CLJ-1301)
;;; [links](http://dev.clojure.org/jira/browse/CLJ-1364)
;;; [to](http://dev.clojure.org/jira/browse/CLJ-1070)
;;; [discussions](http://dev.clojure.org/jira/browse/CLJ-1059) about
;;; the issue.  My current understanding is that there are two
;;; implementations of equality/hashing that the Clojure compiler uses
;;; in differenct circumstances: `equals`/`hashEq` is the Java
;;; implementation of equality/hashing; `equiv`/`hasheq` is the
;;; Clojure one.

#+clj
(deftype GlobalEnv [system-env user-env]
  IGlobalEnv
  (system-env [this]
    system-env)
  (user-env [this]
    user-env)
  clojure.lang.IHashEq
  (hasheq [this]
    (hash-combine system-env user-env))
  clojure.lang.MapEquivalence
  java.io.Serializable
  java.util.Map
  (containsKey [this key]
    (or (.containsKey system-env key)
        (.containsKey user-env key)))
  (containsValue [this val]
    (boolean (or (.containsValue system-env)
                 (.containsValue user-env))))
  (entrySet [this]
    (clojure.set/union (.entrySet system-env) (.entrySet user-env)))
  (equals [this other]
    ;; Pass this on to `equiv`
    (= this other))
  (get [this key]
    (or (get user-env key)
        (get system-env key)))
  (hashCode [this]
    ;; Not sure how this should relate to IHashEq above.  My
    ;; understanding is that the Java hash is used in situations where
    ;; performance is important and the type can be statically
    ;; determined(?). Returning the
    ;; same value from both should be safe, though, if `equals` and
    ;; `equiv` (corresponding to Clojure `=`) are implemented in the
    ;; same way.
    (.hasheq this))
  (isEmpty [this]
    (and (= 0 (count system-env)) (= 0 (count user-env))))
  (keySet [this]
    (clojure.set/union (set (keys system-env)) (set (keys user-env))))
  (size [this]
    (+ (count system-env) (count user-env)))
  (values [this]
    (concat (vals system-env) (vals user-env)))
  clojure.lang.Associative
  clojure.lang.IPersistentCollection
  (clojure.core/equiv [this other]
    (boolean
     (or (identical? this other)
         (when (identical? (class this) (class other))
           (and
            (= system-env (. other -system-env))
            (= user-env (. other -user-env))))))))

;;; For reference, this is the macro-expansion of a `defrecord` form
;;; containing user-env and system-env fields.

#_
(let []
  (deftype*
    GlobalEnv
    revue.util.GlobalEnv
    [system-env user-env __meta __extmap]
    :implements
    [IGlobalEnv
     clojure.lang.IRecord
     clojure.lang.IHashEq
     clojure.lang.IObj
     clojure.lang.ILookup
     clojure.lang.IKeywordLookup
     clojure.lang.IPersistentMap
     java.util.Map
     java.io.Serializable]
    (clojure.core/entrySet
      [this__5942__auto__]
      (set this__5942__auto__))
    (clojure.core/values
      [this__5941__auto__]
      (vals this__5941__auto__))
    (clojure.core/keySet
      [this__5940__auto__]
      (set (keys this__5940__auto__)))
    (clojure.core/clear
      [this__5939__auto__]
      (throw (java.lang.UnsupportedOperationException.)))
    (clojure.core/putAll
      [this__5937__auto__ m__5938__auto__]
      (throw (java.lang.UnsupportedOperationException.)))
    (remove
      [this__5935__auto__ k__5936__auto__]
      (throw (java.lang.UnsupportedOperationException.)))
    (clojure.core/put
      [this__5932__auto__ k__5933__auto__ v__5934__auto__]
      (throw (java.lang.UnsupportedOperationException.)))
    (get
      [this__5930__auto__ k__5931__auto__]
      (.valAt this__5930__auto__ k__5931__auto__))
    (clojure.core/containsValue
      [this__5928__auto__ v__5929__auto__]
      (boolean (some #{v__5929__auto__} (vals this__5928__auto__))))
    (clojure.core/isEmpty
      [this__5927__auto__]
      (= 0 (.count this__5927__auto__)))
    (clojure.core/size
      [this__5926__auto__]
      (.count this__5926__auto__))
    (clojure.core/without
      [this__5924__auto__ k__5925__auto__]
      (if (contains? #{:user-env :system-env} k__5925__auto__)
        (dissoc
          (with-meta (into {} this__5924__auto__) __meta)
          k__5925__auto__)
        (new
          GlobalEnv
          system-env
          user-env
          __meta
          (not-empty (dissoc __extmap k__5925__auto__)))))
    (assoc
      [this__5922__auto__ k__5923__auto__ G__10330]
      (condp identical? k__5923__auto__
        :system-env (new GlobalEnv G__10330 user-env __meta __extmap)
        :user-env (new GlobalEnv system-env G__10330 __meta __extmap)
        (new
          GlobalEnv
          system-env
          user-env
          __meta
          (assoc __extmap k__5923__auto__ G__10330))))
    (clojure.core/iterator
      [this__5921__auto__]
      (clojure.lang.SeqIterator. (.seq this__5921__auto__)))
    (seq
      [this__5920__auto__]
      (seq
        (concat
          [(new clojure.lang.MapEntry :system-env system-env)
           (new clojure.lang.MapEntry :user-env user-env)]
          __extmap)))
    (clojure.core/entryAt
      [this__5916__auto__ k__5917__auto__]
      (let [v__5918__auto__ (.valAt
                              this__5916__auto__
                              k__5917__auto__
                              this__5916__auto__)]
        (when-not (identical? this__5916__auto__ v__5918__auto__)
          (clojure.lang.MapEntry. k__5917__auto__ v__5918__auto__))))
    (clojure.core/containsKey
      [this__5914__auto__ k__5915__auto__]
      (not
        (identical?
          this__5914__auto__
          (.valAt
            this__5914__auto__
            k__5915__auto__
            this__5914__auto__))))
    (clojure.core/equiv
      [this__5913__auto__ G__10330]
      (boolean
        (or
          (identical? this__5913__auto__ G__10330)
          (when (identical?
                  (class this__5913__auto__)
                  (class G__10330))
            (let [G__10330 G__10330]
              (and
                (= system-env (. G__10330 -system-env))
                (= user-env (. G__10330 -user-env))
                (= __extmap (. G__10330 __extmap))))))))
    (cons
      [this__5911__auto__ e__5912__auto__]
      (#'clojure.core/imap-cons this__5911__auto__ e__5912__auto__))
    (empty
      [this__5910__auto__]
      (throw
        (java.lang.UnsupportedOperationException.
          (str "Can't create empty: " "revue.util.GlobalEnv"))))
    (count [this__5909__auto__] (+ 2 (count __extmap)))
    (clojure.core/getLookupThunk
      [this__5907__auto__ k__5908__auto__]
      (let [gclass (class this__5907__auto__)]
        (case
          k__5908__auto__
          :system-env
          (reify
            clojure.lang.ILookupThunk
            (get
              [thunk gtarget]
              (if (identical? (class gtarget) gclass)
                (. gtarget -system-env)
                thunk)))
          :user-env
          (reify
            clojure.lang.ILookupThunk
            (get
              [thunk gtarget]
              (if (identical? (class gtarget) gclass)
                (. gtarget -user-env)
                thunk)))
          nil)))
    (clojure.core/valAt
      [this__5904__auto__ k__5905__auto__ else__5906__auto__]
      (case
        k__5905__auto__
        :system-env
        system-env
        :user-env
        user-env
        (get __extmap k__5905__auto__ else__5906__auto__)))
    (clojure.core/valAt
      [this__5902__auto__ k__5903__auto__]
      (.valAt this__5902__auto__ k__5903__auto__ nil))
    (clojure.core/withMeta
      [this__5901__auto__ G__10330]
      (new GlobalEnv system-env user-env G__10330 __extmap))
    (meta [this__5900__auto__] __meta)
    (clojure.core/equals
      [this__5899__auto__ G__10330]
      (clojure.lang.APersistentMap/mapEquals
        this__5899__auto__
        G__10330))
    (clojure.core/hashCode
      [this__5898__auto__]
      (clojure.lang.APersistentMap/mapHash this__5898__auto__))
    (clojure.core/hasheq
      [this__5897__auto__]
      (bit-xor
        -1798849877
        (clojure.lang.APersistentMap/mapHasheq this__5897__auto__)))
    (system-env [this] system-env)
    (user-env [this] user-env))
  (import revue.util.GlobalEnv)
  (defn ->GlobalEnv
    "Positional factory function for class revue.util.GlobalEnv."
    [system-env user-env]
    (new revue.util.GlobalEnv system-env user-env))
  (defn map->GlobalEnv
    "Factory function for class revue.util.GlobalEnv, taking a map of keywords to field values."
    ([m__5999__auto__]
      (revue.util.GlobalEnv/create
        (if (instance? clojure.lang.MapEquivalence m__5999__auto__)
          m__5999__auto__
          (into {} m__5999__auto__)))))
  revue.util.GlobalEnv)

