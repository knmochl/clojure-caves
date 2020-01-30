(ns caves.entities.silverfish
  (:use [caves.entities.core :only [Entity get-id add-aspect]]
        [caves.entities.aspects.destructible :only [Destructible]]
        [caves.entities.aspects.mobile :only [Mobile move can-move?]]
        [caves.world.core :only [get-entity-at get-tile-kind]]
        [caves.coords :only [neighbors]]))

(defrecord Silverfish [id name glyph color location hp max-hp])

(defn make-silverfish [location]
  (map->Silverfish {:id (get-id)
                    :name "silverfish"
                    :glyph "~"
                    :color :white
                    :location location
                    :hp 12
                    :max-hp 12}))

(extend-type Silverfish Entity
  (tick [this world]
    (let [target (rand-nth (neighbors (:location this)))]
      (if (can-move? this target world)
        (move this target world)
        world))))

(add-aspect Silverfish Mobile
  (can-move? [this dest world]
    (and (#{:floor :wall} (get-tile-kind world dest))
         (not (get-entity-at world dest)))))

(add-aspect Silverfish Destructible)
