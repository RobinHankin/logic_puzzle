## The Einstein puzzle

## In the following list of clues, take

## "7  (9) the person who smokes Pall Mall rears birds: pallmall=oldgold, birds=snails"

## as an example.  This means that clue 7 in the Einstein puzzle
## corresponds to clue 9 in the zebra puzzle; and pallmall in the
## einstein puzzle corresponds to oldgold in the zebra puzzle, and of
## course birds corresponds to snails.  Note that einstein clue 16 has
## no counterpart in the zebra puzzle.


##  2  (2) the Brit lives in the red house
##  3  (3) the Swede keeps dogs as pets: swede = spaniard
##  4  (4) the Dane drinks tea: dane = ukranian
##  5 (13) the green house is on the left of the white house: white=ivory
##  6  (8) the green house's owner drinks coffee
##  7  (9) the person who smokes Pall Mall rears birds: pallmall=oldgold, birds=snails
##  8 (10) the owner of the yellow house smokes Dunhill: dunhill=kools
##  9 (12) the man living in the center house drinks milk
## 10  (5) the Norwegian lives in the first house
## 11 (14) the man who smokes blends lives next to the one who keeps cats: blends=chesterfields, cats=fox
## 12 (15) the man who keeps horses lives next to the man who smokes Dunhill: dunhill=kools
## 13 (11) the owner who smokes BlueMaster drinks beer: bluemaster=luckystrikes, beer=orange
## 14  (6) the German smokes Prince: german=japanese, prince=parliaments
## 15  (7) the Norwegian lives next to the blue house
## 16  (?) the man who smokes blend has a neighbor who drinks water


## Einstein clue 16 corresponds to "chesterfield smoker has a
## neighbour who drinks water" in zebra terminology.

## In the lines below, we see character vectors of length five showing
## the correspondence between the einstein puzzle and the zebra
## puzzle.


# Einstein puzzle:
colour <- c("red","green","white","yellow","blue")               # 1 einstein
##        c("red","green","ivory","yellow","blue")               # 1 zebra

number <- c("one","two","three","four","five")                   # 2 einstein and zebra

nationality <- c("brit"   ,"norwegian","swede"  ,"dane"   ,"german")     # 3 einstein
##             c("english","norwegian","spanish","ukraine","japanese")   # 3 zebra

pet <- c("dogs","birds" ,"cats","horses","fish" )                  # 4 einstein
##     c("dogs","snails","fox" ,"horses","zebra")                  # 4 zebra

drink <- c("tea","coffee","milk","beer"  ,"water")                 # 5 einstein
##       c("tea","coffee","milk","orange","water")                 # 5 zebra

smoke <- c("prince"     ,"pallmall","dunhill","bluemaster","blends"       ) # 6 einstein
##       c("parliaments","oldgold"  ,"kools" ,"luckystrike","chesterfield") # 6 zebra


