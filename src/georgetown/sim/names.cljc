(ns georgetown.sim.names)

(def first-names
  ["Alice" "Amara" "Amelia" "Arthur" "Asher" "Aurora" "Beatrice" "Benjamin"
   "Caleb" "Cara" "Cedric" "Celia" "Charlotte" "Clara" "Cole" "Cora"
   "Daniel" "Daphne" "David" "Delia" "Dorian" "Edith" "Edmund" "Eleanor"
   "Elias" "Ella" "Emmett" "Esther" "Ezra" "Felix" "Fiona" "Flora"
   "Frances" "Frederick" "George" "Grace" "Hannah" "Harriet" "Hazel" "Henry"
   "Hugo" "Ida" "Iris" "Isaac" "Ivy" "James" "Jasper" "Josephine"
   "Julia" "June" "Leah" "Leo" "Lily" "Louisa" "Lucas" "Lucy"
   "Luther" "Mabel" "Marcus" "Margaret" "Martha" "Mary" "Matilda" "Miles"
   "Milo" "Nadia" "Nathaniel" "Nell" "Nora" "Oliver" "Olive" "Oscar"
   "Otto" "Pearl" "Peter" "Phoebe" "Quentin" "Rose" "Ruby" "Rufus"
   "Ruth" "Samuel" "Sarah" "Silas" "Simon" "Sophia" "Stella" "Susannah"
   "Theodore" "Thomas" "Tobias" "Vera" "Victor" "Violet" "Walter" "Wilbur"
   "William" "Willa" "Winifred" "Zane"])

(def last-names
  ["Abbott" "Adler" "Archer" "Ashford" "Bailey" "Baker" "Barlow" "Bennett"
   "Blackwood" "Bowman" "Brooks" "Burton" "Carter" "Chandler" "Clarke" "Coleman"
   "Cooper" "Crane" "Dawson" "Drake" "Dunn" "Ellis" "Fairbanks" "Farley"
   "Fisher" "Fletcher" "Foster" "Fox" "Gardner" "Gibson" "Graham" "Grant"
   "Graves" "Gray" "Hale" "Harper" "Hart" "Hawkins" "Hayes" "Holloway"
   "Holt" "Hopkins" "Howell" "Hudson" "Hunter" "Ingram" "Jennings" "Keating"
   "Kemp" "Lane" "Larkin" "Lawson" "Mason" "Mercer" "Merritt" "Miller"
   "Monroe" "Morgan" "Nash" "Norwood" "Osborne" "Page" "Palmer" "Parker"
   "Pemberton" "Porter" "Potter" "Quimby" "Reed" "Rhodes" "Riley" "Rowe"
   "Sawyer" "Shepherd" "Sherwood" "Slade" "Spencer" "Stone" "Sutton" "Tanner"
   "Thatcher" "Thornton" "Tucker" "Turner" "Underwood" "Vance" "Walker" "Ward"
   "Watts" "Weaver" "Webb" "Wells" "Whitfield" "Wilder" "Winslow" "Wolfe"
   "Woods" "Wright" "Yates" "York"])

(defn random-full-name []
  (str (rand-nth first-names) " " (rand-nth last-names)))

#_(random-full-name)
