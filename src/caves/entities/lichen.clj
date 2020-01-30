(ns caves.entities.lichen
  (:use [caves.entities.core :only [Entity get-id add-aspect]]
        [caves.world.core :only [find-empty-neighbor]]
        [caves.entities.aspects.destructible :only [Destructible]]
        [caves.entities.aspects.receiver :only [send-message-nearby]]))

(defrecord Lichen [id name glyph color location hp max-hp])

(defn make-lichen
  [location]
  (map->Lichen {:id (get-id)
                :name "lichen"
                :glyph "F"
                :color :grey
                :location location
                :hp 6
                :max-hp 6}))

(defn should-grow
  []
  (< (rand) (/ 1 500)))

(defn grow
  [lichen world]
  (if-let [target (find-empty-neighbor world (:location lichen))]
    (let [new-lichen (make-lichen target)
          world (assoc-in world [:entities (:id new-lichen)] new-lichen)
          world (send-message-nearby target "The lichen grows" world)]
      world)
    world))

(extend-type Lichen Entity
  (tick [this world]
    (if (should-grow)
      (grow this world)
      world)))

(add-aspect Lichen Destructible)
