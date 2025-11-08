# Wordle Solver - Haskell
**This tool serves as a guide for solving [Wordle](https://www.nytimes.com/games/wordle/) puzzles!**

As of this project's creation, I am learning the [functional programming](https://en.wikipedia.org/wiki/Functional_programming) language [Haskell](https://www.haskell.org/) in university. Since I've been doing Wordles with friends on Discord for a while now, I got tired of doing them manually and figured I could use my laziness as an opportunity to challenge and improve my Haskell skills by creating a tool that does all the hard work for me!

## Usage
This repo comes with 2 Haskell (`.hs`) scripts: [`wordle_solver`](https://github.com/KojoBailey/wordle-solver-hs/blob/main/wordle_solver.hs) and [`wordle_stats`](https://github.com/KojoBailey/wordle-solver-hs/blob/main/wordle_stats.hs). There are also two text files which contain data that both these scripts access.

`wordle_solver.hs` is the script to run to... well, help solve a Wordle. This should be run with [`wordle-stats.txt`](https://github.com/KojoBailey/wordle-solver-hs/blob/main/wordle-stats.txt) in the same directory, as it relies on this data to predict words.

`wordle_stats.hs` is used to produce `wordle-stats.txt`, and relies on [`valid-wordle-words.txt`](https://github.com/KojoBailey/wordle-solver-hs/blob/main/valid-wordle-words.txt) (which I got from [dracos](https://gist.github.com/dracos/dd0668f281e685bad51479e5acaadb93#file-valid-wordle-words-txt) on GitHub Gist). To use the tool, you don't need to run this yourself, since I already have!

Note that `wordle_solver.hs` won't actually do everything for you. It is only a guide offering advice based on predictions. Therefore, you need to enter your words into Wordle and check with the program turn-by-turn until you (hopefully) reach the solution before running out of turns.

## Examples
The following are example terminal outputs after completing a Wordle puzzle.

### Example 1 - `arise`
```
~~ TURN 1 ~~
Enter word:
pares
Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)
X////
Number of possibilties: 28
(Words with duplicate letters are hidden.)
["soare","share","resay","segar","snare","sewar","stare","scare","sware","reast","arise","resat","arose","shear","aesir","stear","kesar","urase","smear","ursae","swear","skear","resam","resaw","escar","eskar"]

~~ TURN 2 ~~
Enter word:
soare
Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)
/X//=
Number of possibilties: 2
["arise","ursae"]

~~ TURN 3 ~~
Enter word:
arise
Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)
=====

Solution reached in 3 turns!
```

### Example 2 - `peril`
```
~~ TURN 1 ~~
Enter word:
soare
Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)
XXX//
Number of possibilties: 438
(Words with duplicate letters are hidden.)
["riley","mired","pured","tired","piner","cured","mured","buret","curet","fired","biner","piler","miner","ricey","hired","wired","miler","riled","lured","certy","puler","pried","tiler","diner","runed","vired","tuner","finer","liner","biter","citer","cried","miter","ruled","mirex","puter","filer","tried","perky","perdy","rimed","derny","plier","cuter","bider","timer","piker","muter","permy","cider","murex","ferny","viner","riced","ferly","fried","pucer","liter","riped","biker","dimer","tiger","pizer","mercy","viler","puker","rived","cruet","niter","dicer","trued","pervy","wried","kiter","jurel","merit","rivet","luter","cuber","ripen","mixer","lurex","prief","derpy","tyred","giber","diker","peril","liger","tuber","fiber","rumen","buyer","mpret","diver","liber","germy","hider","grued","fumer","flier","giver","hiper","brief","liker","riven","cruel","nicer","bluer","duper","fiver","liver","wider","tuyer","niger","urned","nerdy","derby","meril","gyred","rivel","mbret","hiker","wiper","lifer","luger","berth","hiver","fixer","rubel","reify","huger","wiver","viper","nuder","herby","jerky","gruel","redly","grief","twier","gluer","nixer","jiber","reply","relit","threw","tyler","plyer","derth","luxer","pyrex","nervy","jiver","unred","jerid","yrneh","refly","perch","predy","derig","premy","remit","urged","urped","rebuy","retin","merch","reink","cyder","renig","perdu","crepy","recit","rebid","breid","flyer","irked","reign","cyber","rebit","reing","beryl","prent","ryked","hertz","relic","nertz","repin","ulcer","brent","refit","prexy","fermi","trend","crein","vertu","rewin","retip","twyer","inter","recti","gyver","idler","hyper","freit","upter","drent","rewth","xeric","rendu","recut","rebud","demur","grein","reput","remix","under","redig","redip","rebut","rheid","femur","lemur","preif","kefir","retch","their","redif","crept","urbex","debur","repun","inker","gebur","ureid","inver","vezir","umber","treif","infer","refix","icker","fremd","rejig","urent","yrent","ither","redug","preux","redux","redub","treck","fleur","ering","dreck","kreng","ureic","erupt","eruct","erick","wreck","ervil","treyf","rheum"]

~~ TURN 2 ~~
Enter word:
riley
Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)
////X
Number of possibilties: 3
["peril","meril","ervil"]

~~ TURN 3 ~~
Enter word:
peril
Enter correctness ('=' : correct, '/' : wrong position, 'X' : not used)
=====
Number of possibilties: 1
["peril"]

Solution reached in 3 turs!
```

## Known issues
I still need to add detection for when you input a word with duplicate letters but the target does not have all of them - for example, you input "deeps" but the answer is "slept". Wordle will show this as "XX==/", but inputting this into the program will make it think that "e" is not in the word at all, even though it is but with only one instance. Until this is fixed, you'd need to change your input to "X/==/", which isn't perfect, but it keeps it working at least.
