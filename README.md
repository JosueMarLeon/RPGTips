Notice
================

This is a mirror of the deleted RPGTips repository by rpg-tips, as it existed on 2020-06-07.

To install, use the following commands:

`install.packages("devtools")`

`library(devtools)`

`install_github("JosueMarLeon/RPGTips")`

The original README is available below.


RPG Tips
================

This package is a compilation of functions in R to make RStudio the optimal RPGing environment.

Included games
==============

These are the products included, as well as the prefix of the functions that call their tables.

-   [Mythic Variations 2](https://www.wordmillgames.com/mythic-variations-2.html): `gme`.
-   [The Adventure Crafter](https://www.wordmillgames.com/the-adventure-crafter.html): `gme`.
-   [The Universal NPC Emulator](https://www.drivethrurpg.com/product/134163/UNE-The-Universal-NPC-Emulator-rev): `une`.
-   [Game Master's Apprentice](https://www.drivethrucards.com/product/125685/The-GameMasters-Apprentice-Base-Deck): `gma`.
-   [Ironsworn (with some tables from Delve too!)](https://www.ironswornrpg.com/): `iron`.
-   [Icons Superpowered Roleplaying](https://www.drivethrurpg.com/product/131765/ICONS-Superpowered-Roleplaying-The-Assembled-Edition): `ìcons`.
-   [Instant Game](https://rpggeek.com/rpg/3837/instant-game): `inst`.
-   D&D 5e Dungeon Master's Guide: `dmg`.
-   [Into the Wyrd and Wild](https://www.drivethrurpg.com/product/274922/Into-the-Wyrd-and-Wild): `wyrd`.
-   [The Perilous Wilds](https://www.drivethrurpg.com/product/156979/The-Perilous-Wilds): `perilous`.
-   [Solo Adventurer's Toolbox](https://www.dmsguild.com/product/252355/The-Solo-Adventurers-Toolbox): `toolbox`.
-   [Solo Investigator's Handbook](https://www.dmsguild.com/product/252355/The-Solo-Adventurers-Toolbox): `handbook`.
- [Random Terrain and Encounter Generator](https://www.drivethrurpg.com/product/250641/Random-Terrain-and-Encounter-Generator): `bloch`.

Some additional goodies:
- PC Options: backgrounds extracted from 5e and PF 2e: `pc_background`.
- Random monster generators: `monster_osr` for OSR D&D monsters and `monster_path` for monsters from Pathfinder 1e.
- Settlements stuff: `settlement`, to create size, name and government type of a place. 

How to install
==============

As this is not on CRAN and it won´t be for a lot of time, you best bet is to use "install_github"
function from the "devtools" package:

-   install devtools

`install.packages("devtools")`

-   load RPGtips

`library(devtools)`

`install_github("rpg-tips/RPGTips")`

How to use it
=============

The different functions and generators are divided by preffixes:

`gme` for Mythic-related functions. Some examples:

-   Fate Check:

``` r
gme_fate_check()
```

    ## [1] "Exceptional No"

-   Detail Check:

``` r
gme_detail_check()
```

    ## [1] "Focus PC"

-   Actions random table:

``` r
gme_actions_table()
```

    ## [1] "Attach Art"

-   Turning Point:

``` r
gme_turning_point()
```

    ##      theme                     point
    ## 1  tension          Into the Unknown
    ## 2  tension (Meta) Character Steps Up
    ## 3 personal                Conclusion
    ## 4 personal                      None
    ## 5  mystery           The Hidden Hand

`iron` for Ironsworn-related functions. Some examples:

-   Dice roll:

``` r
iron_dice()
```

    ## [1] "Miss"

-   Combat actions:

``` r
iron_combat_actions()
```

    ## [1] "Attack with precision"

-   NPC disposition:

``` r
iron_npc_disposition()
```

    ## [1] "Helpful"

There are two additional auxiliar functions, `roll` and `roll_table`.

-   `roll` allows to emulate any typical dice roll:

``` r
roll(n = 1, d = 6)
```

    ## [1] 3

``` r
roll(n = 4, d = 6, type = "drop_min")
```

    ## [1] 11

``` r
roll(n = 2, d = 20, type = "advantage")
```

    ## [1] 16

``` r
roll(n = 2, d = 20, type = "disadvantage")
```

    ## [1] 4

``` r
roll(type = "fate")
```

    ## [1] 2

-   `roll_table` is created to keep results consistent when they are written on an Rmarkdown file, as a workaround to setting a seed everytime we want to roll on a specific table. You just need to set your `seed_counter` variable to whatever you want at the start of your document, and then call `roll_table` to roll on a generator and it will handle everything.

``` r
seed_counter <- 7777
roll_table(gme_actions_table)
```

    ## [1] "Fight Animals"

``` r
roll_table(gme_event_check)
```

    ## [1] "Move Away from a Thread"

``` r
roll_table(iron_npc_goal)
```

    ## [1] "Rebel against power"

``` r
roll_table(iron_dice)
```

    ## [1] "Miss"
