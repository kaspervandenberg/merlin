;;; Names for characters from the Arthur saga

(in-package :merlin)

(defparameter *genders* '(:male :female))

(defparameter *names* 
  '((:male . 
     (Afallach Afallech Accalon Accolon Addanc Aglouale Agravain Agrawain Alain 
      Albion Alexander Alis Amfortas Ancelot Andret Anguish Anir Antor Apollo 
      Arawn Aroundight Artegal Arthgal Arthgallo Arthur Arthwys Artur Artus 
      Augusel Auguselus Avallach Avanc Awarnach
      Bagdemagus Balan Baldulf Balin Ban Beaumains Bedivere Bedver Bellamour 
      Bellangère Bersules Bicoir Bladud Blaise Blamor Blamour Bleoberis Bliant 
      Blyant Bohort Bors Bran Brandelis Brangoire Branor Bredbeddle Brennus 
      Breunor Briefbras Bruce
      Cabal Cador Cadwaladr Cadwallon Cadwr Caerleon Cai Caliburn Calogrenant 
      Camlann Caractacus Caradawc Caradawg Caradoc Caradwg Carlisle Carnwennan 
      |Cath Palug| Cavall Cawr |Cawr-Madog| Chapalu Clamedeus Clarent Claudas 
      Colgrevance Constantine Culhwch Cunobelinus Custennin Cymbeline Cynfarch
      Cynyr
      Dagonet 
      ))
    (:female .
     (Acheflour Ade Ana Angharad Angharat Angharawd Anglides Anna Argante 
      Astolat Avalon
      Belakane Blanchefleur Blancheflor Brangaine Brengwain Bronwen
      Chelinda Chelinde Clarine Clarissant))))


(defun random-elt (choices)
  "Select a random element from `choices`."
  (elt choices (random (length choices))))


(cl-ecs:defcomponent character-identity (gender name))


(defun add-random-character-identity (entity)
  "Add `character-identity` component to an entity."
  (unless (member 'character-identity (cl-ecs::entity-components entity))
    (let* ((g (random-elt *genders*))
           (n (random-elt (assoc g *names*))))
      (cl-ecs:add-component entity 'character-identity `(:character-identity/gender ,g :character-identity/name ,n)))))



