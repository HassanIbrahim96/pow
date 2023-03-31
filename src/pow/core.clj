(ns pow.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clj-audio.core :refer [->stream play decode]]))

(def screen-x 500)
(def screen-y 500)

(def max-y (- screen-y 51))
(def max-x (- screen-x 51))
(def x-edge1 10)
(def x-edge2 440)
(def y-edge1 9)
(def y-edge2 439)
(def padded-x 20)

(def jump-power 100)

(def slide-power 10)

(def projectile-speed 10)

(def square-height 50)
(def square-width 50)

(def projectile-delay 370)

(def init {:rect {:x 0 :y max-y}
           :keys-pressed #{}
           :bullets {}
           :monsters {}
           :score 0})

(defn play-sound [filename]
  (future (-> (->stream filename)
              decode
              play)))

(defn monster-hit []
  ;(play-sound "monster-hit.wav")
  )

(defn bullet-shoot []
  (play-sound "judy.mp3"))

(defn rect-circle-intersect? [{:keys [r c]}]
  (let [circle-distance {:x (Math/abs (- (:x c) (:x r)))
                         :y (Math/abs (- (:y c) (:y r)))}]
    (cond (or (> (:x circle-distance)
                 (+ (:radius c) (/ (:size r) 2)))
              (> (:y circle-distance)
                 (+ (:radius c) (/ (:size r) 2))))
          false
          (or (<= (:x circle-distance)
                  (/ (:size r) 2))
              (<= (:y circle-distance)
                  (/ (:size r) 2)))
          true
          :else (let [sq (+ (Math/pow (- (:x circle-distance)
                                         (/ (:size r) 2))
                                      2)
                            (Math/pow (- (:y circle-distance)
                                         (/ (:size r) 2))
                                      2))]
                  (<= sq (Math/pow (:radius c) 2))))))

(defn rectangular-intersect? [{:keys [r1 r2]}]
  (and (>= (+ (:x r1) (:size r1))
           (:x r2))
       (<= (:x r1)
           (+ (:x r2)
              (:size r2)))
       (>= (+ (:y r1)
              (:size r1))
           (:y r2))
       (<= (:y r1)
           (+ (:y r2) (:size r2)))))

(defn bullets-intersect? [{:keys [state x y width height]}]
  (let [bullets (:bullets state)]
    (loop [bullets bullets
           intersect? false
           bullet-id nil]
      (if (or intersect? (empty? bullets))
        intersect?
        (let [[id bullet] (first bullets)
              intersect (rect-circle-intersect?
                         {:r {:x x
                              :y y
                              :size width}
                          :c {:x (:x bullet)
                              :y (:y bullet)
                              :radius 15}})]
          (recur (rest bullets)
                 intersect
                 (if intersect
                   id
                   nil)))))))

(defn monsters-intersect? [{:keys [state x y
                                   width height]}]
  (let [monsters (:monsters state)]
    (loop [monsters monsters
           intersect? false]
      (if (or intersect? (empty? monsters))
        intersect?
        (let [[id monster] (first monsters)
              intersect (rectangular-intersect?
                         {:r1 {:x x
                               :y y
                               :size square-width}
                          :r2 {:x (:x monster)
                               :y (:y monster)
                               :size square-width}})]
          (recur (rest monsters)
                 intersect))))))

(defn monsters-intersect-bullet? [{:keys [state x y]}]
  (let [monsters (:monsters state)]
    (loop [monsters monsters
           intersect? false]
      (if (or intersect? (empty? monsters))
        intersect?
        (let [[id monster] (first monsters)
              bx (Math/abs (- x (:x monster)))
              by (Math/abs (- y (:y monster)))
              cr 15]
          (recur (rest monsters)
                 (cond (> bx (+ cr (/ square-width 2)))
                       false
                       (> by (+ cr (/ square-height 2)))
                       false
                       (<= bx (/ square-width 2))
                       true
                       (<= by (/ square-height 2))
                       true
                       :else (let [distance (+ (Math/pow (- bx (/ square-width 2)) 2.0)
                                               (Math/pow (- by (/ square-height 2)) 2.0))]
                               (<= distance (Math/pow 15 2.0))))))))))

(def last-bullet-id (atom nil))

(defn get-last-bullet-id [state]
  (reset! last-bullet-id (q/millis)))

(defn shoot [state]
  (let [current-millis (q/millis)]
    (if (and @last-bullet-id
             (< (- current-millis @last-bullet-id)
                projectile-delay))
      state
      (let [x (:x (:rect state))
            y (:y (:rect state))
            id (get-last-bullet-id state)]
        (bullet-shoot)
        (assoc-in state [:bullets id] {:x x :y y})))))

(defn draw-monsters [state]
  (doall (for [id (range 0 30)]
           (if-let [monster (get (:monsters state) id)]
             (do
               (when-not (:defeated? monster)
                 (if (:rgb monster)
                   (apply q/fill (:rgb monster))
                   (q/fill 255 0 0))
                 (q/rect (:x monster) (:y monster) 50 50)))
             (do
               (q/fill 255 0 0)
               (q/rect (/ screen-x 2) 0 50 50))))))

(defn update-state [state]
  (if (:game-over? state)
    (if (:space (:keys-pressed state))
      init
      state)
    (let [key (q/key-as-keyword)
          keys-pressed (:keys-pressed state)
          new-x (if (or (:d keys-pressed)
                        (:a keys-pressed))
                  (if (:d keys-pressed)
                    (+ (:x (:rect state)) slide-power)
                    (- (:x (:rect state)) slide-power))
                  (:x (:rect state)))
          new-y (if (or (:w keys-pressed)
                        (:s keys-pressed))
                  (if (:w keys-pressed)
                    (- (:y (:rect state)) slide-power)
                    (+ (:y (:rect state)) slide-power))
                  (:y (:rect state)))
          adjusted-x (if (or (<= new-x 0)
                             (>= new-x max-x))
                       (:x (:rect state))
                       new-x)
          adjusted-y (if (or (<= new-y -1)
                             (>= new-y max-y))
                       (:y (:rect state))
                       new-y)
          game-over? (monsters-intersect? {:state state
                                           :x adjusted-x
                                           :y adjusted-y
                                           :width square-width
                                           :height square-height})
          mons (into {} (doall (for [id (range 0 30)]
                                 (let [monster (get (:monsters state) id)]
                                   (if (:defeated? monster)
                                     {id monster}
                                     (if monster
                                       (let [new-direction (cond (= (:x monster)
                                                                    x-edge2)
                                                                 :left
                                                                 (= (:x monster)
                                                                    x-edge1)
                                                                 :right
                                                                 :else (:direction monster))
                                             new-y-direction (cond (= (:y monster)
                                                                      y-edge1)
                                                                   :down
                                                                   (= (:y monster)
                                                                      y-edge2)
                                                                   :up
                                                                   :else (:direction-y monster))
                                             new-x (if (= new-direction :right)
                                                     (min (+ (:x monster) (rand-int 10)) x-edge2)
                                                     (max (- (:x monster) (rand-int 10)) x-edge1))
                                             new-y (if (= new-y-direction :up)
                                                     (max (- (:y monster) (rand-int 10)) y-edge1)
                                                     (min (+ (:y monster) (rand-int 10)) y-edge2))
                                             defeated? (bullets-intersect? {:state state
                                                                            :x (:x monster)
                                                                            :y (:y monster)
                                                                            :width square-width
                                                                            :height square-height})]
                                         (when defeated?
                                           (monster-hit))
                                         {id {:x new-x
                                              :y new-y
                                              :direction new-direction
                                              :direction-y new-y-direction
                                              :defeated? defeated?
                                              :rgb (:rgb monster)}}
                                         )
                                       {id {:x (/ screen-x 2)
                                            :y 0
                                            :direction-y :down
                                            :direction :right
                                            :rgb [(rand-int 255) (rand-int 255) (rand-int 255)]}}))))))
          bullets (into {}
                        (remove nil? (doall (for [[id {:keys [x y]}] (:bullets state)]
                                              (when-not (monsters-intersect-bullet?
                                                            {:state state
                                                             :x x
                                                             :y y})
                                                {id {:x x
                                                     :y (- y projectile-speed)}})))))
          score (count (filter :defeated? (vals mons)))]
      (merge state
             {:game-over? game-over?
              :rect {:x adjusted-x
                     :y adjusted-y}
              :bullets bullets
              :monsters mons
              :score score}))))

(defn draw-player [state]
  (let [x (:x (:rect state))
        y (:y (:rect state))]
    (q/no-stroke)
    (q/fill 0 255 255)
    (q/rect x y 50 50)))

(defn draw-bullets [state]
  (let [bullets (:bullets state)]
    (doseq [[id {:keys [x y]}] bullets]
      (q/stroke 0 255 255)
      (q/stroke-weight 15)
      (q/fill 255 0 0)
      (q/point x y))))

(defn draw-score [state]
  (q/fill 255 0 0)
  (q/text (str "Score: " (:score state)) 50 50))

(defn draw-state [state]
  ;; (if (:game-over? state)
  ;;   (do (q/background 255)
  ;;       (q/fill 255 0 0)
  ;;       (q/text "GAME OVER" 230 250)))
  (do
    (q/background 240)
    (draw-score state)
    (draw-bullets state)
    (draw-player state)
    (draw-monsters state)))

(defn setup []        
  (q/frame-rate 120)
  (q/color-mode :hsb)
  init)

(q/defsketch pow
  :title "POW!"
  :size [screen-x screen-y]
  :setup setup
  :draw draw-state
  :update update-state
  :key-pressed (fn [state pressed-key]
                 (let [key (:key pressed-key)]
                   (if (= key :up)
                     (shoot state)
                     (assoc state :keys-pressed
                            (conj (:keys-pressed state)
                                  key)))))
  :key-released (fn [state released-key]
                  (let [key (:key released-key)]
                    (assoc state :keys-pressed
                           (disj (:keys-pressed state)
                                 key))))
  :features [:keep-on-top]
  :middleware [m/fun-mode])
