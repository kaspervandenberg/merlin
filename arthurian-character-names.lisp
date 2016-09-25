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
      Dagonet Daguenet Dinadan |Dinas Emrys| Dodinel Druas Drudwyn Drust Drystan
      Ector Eliaures Eliavrès Elidure Elyan Emrys Engrès Erbin Erec Esclados 
      Escorant Evadeam Evelake Evrain Evrawg Excalibur
      Falerin Feirefiz Flollo Florence
      Gahariet Gaheris Gahmuret Galahad Galahalt Galatine Galehot Galeron Gareth 
      Garlon Gauvain Gawain Geraint Giflet Girard Girflet Glais Glatisant Glewlwyd 
      Goreu Gorlassar Gorlois Gouvernail Griflet Gringolet Guerehes Guiomar 
      Guivret Guy Gwalchaved Gwalchmai Gwernach Gwion Gwyn Gwynham 
      Hebron Hector Helaine Hellekin Hengist Hengroen Hodain Hoel Horsa Howel 
      Huarwar Huon Hywel
      Ironside Isdernus Iwain
      Johfrit
      Kadyriath Kaherdin Kai Kardeiz Kay Keridak Keu Kilhwch Kilydd Kyner Kynthelig 
      Lac Lailoken Lamorak Lancelot Laodegan Launcelot Launfal Leodegan 
      Leodegraunce Leodogran Lionel Lionell Llacheu Llew Lluagor Lludd Llychlyn 
      Llyr Lohengrin Lohoot Lohot Lot Lucan Lucas Lucius
      Mabon Mabonagrain Mabuz Madoc Mador Maheloas Malduc March Maris Mark Marrok 
      Medrod Meleagant Meliadus Melwas Menw Merddhin Modred Moraunt Mordred 
      Morholt
      Nascien Nentres
      Octha Owain Ozanna
      Palamedes Palomides Pant Parzival Passebreul Pellam Pellean Pelleas Pellehan 
      Pellehem Pelles Pellinore Pendragon Perceforest Percival Percivale 
      Percyvelle Peredur Perimones Phelot Pheredin
      Rhongomyant Rhydderch Rhyence Rience Rion Rivalen Ryence
      Saffire Safir Sagramour Sagremor Segwarides Spumador
      Tallwch Teithi |Tom a Lincoln| Tor Trevrizent Tristan Tristram Turquine 
      |Twrch Trwyth|
      Uchtryd Urien Uriens Uther Uwaine
      Valerin Vortigern Vortimer
      Walganus
      Yder Yspaddaden Yueins Yvain Yvaine Ywain Ywaine))
    (:female .
     (Acheflour Ade Ana Angharad Angharat Angharawd Anglides Anna Argante Astolat 
      Avalon
      Belakane Blanchefleur Blancheflor Brangaine Brengwain Bronwen
      Chelinda Chelinde Clarine Clarissant Condwiramur Condwiramurs Creirdyddlydd 
      Cundrie
      Danbrann Dechtere Dindrane
      Eigyr Elaine Elizabeth Elsa Enid Enide Enygeus Essylt Estrild Esyllt Ettard 
      Ettarre
      Fenice Floree
      Galiene Ganieda Goleuðyð Grisandole Guenever Guenloie Guinever Guinevere 
      Gwenddydd Gwendolen Gwendoloena Gwenever Gwenhwyfach Gwenhwyfar Gwenhwyvach 
      Gwenhwyvar Gyneth
      Helaine Herzeloyde
      Iblis Igerne Igraine Isabella Iseult Ishild Isolde
      Kundry
      Lamri Laudine Linet Linette Lisanor Llamrei Luned Lunete Lynette Lyonés 
      Lyonesse Lyonors
      Margawse Matilda Modron Morcades Morgan Morgause Morvudd Morvydd
      Nimue Nineve
      Olwen Orcades Orguelleuse
      Prydwen
      Sebille Shalott Sigune Soredamor Soredamors
      Tegan Tegau
      Viviana Vivien
      Ygerne Ygraine Yseulte))))


(defun random-elt (choices)
  "Select a random element from `choices`."
  (elt choices (random (length choices))))


(defclass Character-Identity (Component)
  ((gender :initarg :gender
           :reader gender)
   (name :initarg :name 
         :reader name)))


(defun add-random-character-identity (entity)
  "Add `character-identity` component to an entity."
  (with-existing-entity 
    entity
    (lambda (entity) 
      (unless (all-components-of entity 'Character-Identity) 
        (let* ((g (random-elt *genders*)) 
               (n (random-elt (assoc g *names*)))) 
          (entity-add-fresh-component entity 'Character-Identity :gender g :name n))))))



