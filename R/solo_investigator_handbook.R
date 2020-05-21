#' @export
handbook_scenario_problem <- function(){
  # Wall of text ----
  raw <- "Important relic or item lost. Book stolen?
The town water supply has dried up and no-one knows why
Environmental disturbance / Strange Weather / Infestation / Plague
You discover a map or rumours of a great mystery hitherto undiscovered
You are passing through a town and notice that the locals are acting a little too nicely towards you. Almost as if they do not want you to leave.
A convoy passes near your house, many military vehicles, and a large truck which has all the windows blacked out. What are they carrying in there?
Inhabitants of a town are becoming diseased / going mad / dying mysteriously
Investigator / notable figure left some time ago on a journey but has not returned.
A photographer claims to have captured an image of a being hitherto never recorded before.
Your most trusted friend gives you a package and implores you to take it to such and such a person. He also tells you not to open said package, but tells you that his life, and the lives of thousands, depend on you getting this package to its destination.
You awake with a start, and initially everything is fine. Until you go to the bathroom mirror and see that overnight your body has become covered with strange tattoos - glyphs and runes of some sort! How did these get here? Some of them have even been scarified into your skin. They are all over your body, and it will be difficult for you to go out in public.
A close friend is arrested and detained in a top secret facility. The friend manages to get word to you just before they are taken
A remote site or town has been devastated by a mysterious phenomenon. You have been asked to travel there and investigate.
You keep having the same recurring, disturbing dream, accompanied by very specific information about a particular location/time. How has this information come into your brain?
A strange noise is coming from somewhere, a tapping or groaning or something... where are you and why is this happening?
A noted person has been kidnapped.
A relative dies, leaving you his estate, an old house, and some strange items.
While travelling from A to B, your transport crashes and you are stranded in the wilderness. You discover a strange structure.
Rumours of a lost temple to the Old Ones.
You and a friend uncover a bizarre mystery in a dusty library. The next day, your friend winds up dead.
You awake with no memory of the last 24 / 48 hours. (Optional: a few moments later, there is a loud banging on your door)
You are on holiday in a remote location when it begins to happen. A strange creeping feeling that you cannot pin down, but all is not well.
Through your connections, you learn that a strange item has come up for auction. No one knows where it comes from, but interest is hot.
There is an odd smell in your house / Your pet is acting weird towards a particular wall/room in your house.
Due to a shipwreck / storm you are forced onto an island with an old dilapidated hut on it.
You find an odd object in the basement of your house, possibly while excavating.
A scientist friend tells you he has made a miraculous discovery regarding other dimensions. He wants to meet with you. Then, a day later, he mysteriously disappears.
You discover that a relative's house is much older than originally thought, and has a fascinating history
A friend begins losing their mind, wasting away, and they are raving about some truly outlandish things.
A friend invites you to come and spend a night with him in a house where he has observed paranormal phenomena. He needs someone else to confirm he is not insane.
Strange beings have been sighted in the harbour by local fishermen and sailors
A painting that has passed through several hands comes up for sale. It is rumoured that everyone who has owned it has died from being choked to death, although no killer has ever been found. Who was the creator?
A local politician begins acting very strange, then goes mad. He ends up murdering his own family but protests his innocence endlessly. After exhausting all leads, a representative comes to you offering a large sum of money to help clear the politician's name by getting to the bottom of the mystery. Is he guilty?
Heading to work one morning, noone recognizes you. This is disconcerting, but even more so when your family and friends don't recognize you either.
An old cold case comes to your attention. A girl who died in mysterious circumstances near a lake in your home town. You find a new piece of evidence.
A movie is showing at the local cinema. The strange thing is, everyone who goes and sees it has the same recurring nightmare and ends up going insane. When the makers of the film are researched, it leads to a dead end.
A disturbed war veteran claims that during his tour in Gallipoli / The Somme (insert WWI location here) he saw strange creatures on the opposite side, interacting with the enemy in some way.
A geologist is in the news raving about discovering a new mineral in a local cave system
While on a plane voyage over the ocean, you happen to look down and see the shape of a colossal beast breaching the water's surface. You are tens of thousands of feet up in the air yet this behemoth is still visible. The sight shakes you to the core. You feel compelled to tell someone before you go insane, or at least uncover some sort of corroborative information or other accounts of this creature.
While on holiday at your family's house by the lake, you discover a strange, black structure/formation in the middle of the forest.
A ship-wreck survivor appears on the local beach with a disturbing tale.
A mute child befriends you. You have no idea where this child comes from, but they have strange markings on their body.
A friend recently returned from a holiday abroad, gives you a gorgeous little figurine to place on your mantlepiece. However, every morning when you wake up, the figurine has moved to some new place in your house.
A prisoner escapes, one with a history of occult dabblings and sacrifices. A high priest/priestess.
While speaking to a random acquaintance, they suddenly adopt a different voice and bearing, and deliver you a message while in a sort of trance. Then they emerge from the trance with absolutely no memory of what just happened.
You dream of a horrific event near your town and are then horrified to read about the exact event (with possibly a few details added) the next morning in the local newspaper.
Bodies have been going missing from the local graveyard.
An gallery stages an exhibition of an artist who died recently. The images on display are declared brilliant, revolutionary and daring by art critics. You find them highly disturbing and wonder what caused the artist to create these images. More than that, do they hold some clue to the artist's death?
You are on a train, heading to your destination. The train leaves town and enters beautiful countryside. You drift off into a pleasant and peaceful sleep. Some hours later you awake to find the train empty and stopped on the track. You are the only passenger. Everyone, including the driver, is gone."
  # Processing ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_scenario_source <- function(){
  # Wall of text ----
  raw <- "Directly told by a friend / acquaintance
Note / scrap of information / historical notes
News report
Overheard conversation
Weird portent / dream / oracle
Directly observing some bizarre occurence
A rich benefactor requesting your help in return for payment
The police (or other authority) requesting your help and expertise
An old family heirloom falls and breaks, revealing something mysterious inside
Investigator has done something bad, and someone knows. They threaten to blackmail them unless they do this thing..."
  # Processing ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_location <- function(){
  # Wall of text ----
  raw <- "City
Jungle
Mountains
Sea
Forest
Desert
Polar Region
Swamp
Farmland / Rural
Excluded Zone"
  # Processing ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_location_urban <- function(){
  # Wall of text ----
  raw <- "Library
Mansion
Police Station
Asylum
Abandoned building
Weapons dealer
Municipal Park
Hotel
Museum
Gentleman's Club
Townhouse
Bank
Shop/Store
Significant monument
Uknown building
Art Gallery
Hospital
Morgue
Train Station
University"
  # Processing ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_occupation <- function(){
  # Wall of text ----
  raw <- "Antiquarian
  Artist
  Athlete
  Author
  Clergyman
  Criminal / Mafioso
  Dilletante
  Doctor of Medicine
  Engineer
  Entertainer
  Farmer / Woodsman
  Foot Soldier
  Hobo
  Intelligence Agent
  Journalist
  Lawyer
  Librarian
  Military Commander
  Misionary
  Musician
  Parapsychologist
  Police Detective
  Police Patrolman
  Private Investigator
  Professor
  Revolutionary
  Archaeologist
  Historian"
  # Processing ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_story_direction <- function(plus = 0){
  res <- sample(1:100, 1) + plus
  if(res < 21) return('Downtime')
  if(res < 41) return('Development')
  if(res < 61) return('Discovery')
  if(res < 81) return('Danger')
  else return('Dice Roll')
}

#' @export
handbook_downtime <- function(plus = 0){
  res <- sample(1:100, 1) + plus
  if(res < 81) return('You are left undisturbed')
  if(res < 91) return('Mild disturbance - a vision, a dream, hearing something.')
  else return('A potentially shocking disturbance. Go to Dice Roll table.')
}

#' @export
handbook_development <- function(plus = 0){
  res <- sample(1:100, 1) + plus
  if(res < 11) return('Self-help: You access your inner resources, perhaps related to your backstory. You feel a surge of inner resolve. Make a Sanity Roll,
regaining 1d6 Sanity if successful, losing 1 Sanity if not.')
  if(res < 21) return('You meet a friendly NPC (roll on NPC tables, re-rolling for negative Keyword results).')
  if(res < 31) return('Roll on Verbs table.')
  if(res < 41) return('Roll on Random Events Table.')
  if(res < 51) return("You are met by a seemingly neutral PC. Example: A cop on the beat, a wandering local, an inquisitive child. The keyword here is seemingly. They probably are neutral, but possibly not. Perhaps they are secretly aiding you, or opposing you. You don't know yet.")
  if(res < 61) return('Neutral event. Mundane life presents itself in some way. People go about their daily routines, blind to the events that you are witnessing.')
  if(res < 71) return('You find/observe/hear something that tests your Sanity. Make four rolls on the Verbs table, then refer to the Mythos Table to gauge the appropriate level of the event.')
  if(res < 81) return('You meet a hostile NPC (roll on NPC tables, re-rolling for positive Keyword results).')
  if(res < 91) return('Something awful happens to you. Perhaps you are attacked, or captured, or fall victim to some heinous trap. Make four rolls on the Verbs table, interpreting negatively, then refer to the Mythos Table to gauge the appropriate level of the event.')
  elsereturn('A barefaced attempt on your life.')
  }

#' @export
handbook_discovery <- function(){
  res <- sample(1:100, 1)
  if(res < 11) return('You discover a clue, or something directly related to the uncovering of the mystery. Roll on Clue Table or Verbs table (your choice). With the context of your Skill Roll, this should tell you what you have discovered.')
  if(res < 21) return('Through research/revelation/memory spark, you link two pieces of information you have learnt.')
  if(res < 31) return('Someone slips you/ leaves you a note pointing you in the right direction.')
  if(res < 41) return('You observe/are caught up in some occurrence directly relating to what you are investigating')
  if(res < 51) return(" A news report relating to your case.")
  if(res < 61) return('You overhear someone discussing the thing you are researching. It may be indirectly related, but the discussion is definitely of interest to you. Or are you just hearing things? Make a Q/A roll or use Rumours Table.')
  if(res < 71) return('Your memory is sparked regarding something you had heard regarding an aspect of the case, some time ago.')
  if(res < 81) return('You run into someone, possibly a relative of someone who fell victim to this mystery. (Roll on NPC tables). This has reached further than you previously thought...')
  if(res < 91) return('Someone with knowledge of the case comes to you with a warning..')
  else return('Something you previously presumed about the case turns out to be wrong. The truth regarding this aspect of the case is revealed.')
}

#' @export
handbook_danger <- function(){
  res <- sample(1:100, 1)
  if(res < 9) return('You are being stalked/followed by someone. A mysterious character who you have perhaps seen before, but perhaps not.')
  if(res < 17) return('You find a threatening note.')
  if(res < 26) return('You feel a threatening presence. It is tangible, a spirit, or something malevolent nearby. The Verbs Table can add more detail here.')
  if(res < 34) return('Out of nowhere, you are assaulted by an unknown thug.')
  if(res < 43) return("You receive a disturbing phone call or phone message. If neither of these is appropriate, you either pick up or are related a piece of information, possibly by an NPC. Verbs Table to determine what.")
  if(res < 52) return('Some sort of bizarre affliction overtakes you.')
  if(res < 61) return('An old nemesis or problem from your past resurfaces. Relevance to your case can be deduced with Q/A rolls or consulting the Verbs table. Even if the problem is unconnected to the story, it may influence it.')
  if(res < 70) return('The weight of all you have discovered weighs down on you. Make a sanity roll if things have been bad. 0/1d4.')
  if(res < 79) return('Your apartment/place of work has been tossed, searched. What were they looking for? Were you there when it happened.')
  if(res < 88) return('An attempt on your life.')
  if(res < 79) return('A friend is attacked severely / killed.')
  else return('You encounter a Mythos creature.')
}

#' @export
handbook_dice_roll <- function(){
  res <- sample(1:100, 1)
  if(res < 31) return('You hear something. Go to Auditory Effect table.')
  if(res < 61) return('You see something. Go to Visual Effect table.')
  else return('Something happens. Random Event Table.')
}

#' @export
handbook_npc <- function(){
  # Occupations ----
  occupation <- "Antiquarian
Journalist
Servant / Butler
Artist
Athlete
Author
Clergyman
Criminal
Dilletante
Doctor of Medicine
Entertainer
Engineer
Police Patrolman
Crone
Farmer
Foot Soldier
Hobo
Intelligence Agent
Gypsy
Hermit
Warden
Lawyer
Librarian
Military Commander
Missionary
Musician
Parapsychologist
Police Detective
Private Investigator
Professor
Revolutionary
Archaeologist
Historian
Drifter
Savant
Caretaker
Ascetic
High Priest
Soldier
Assassin
Gravedigger
Henchman
Restauranteur
Aristocrat
Tradesman
Thug
Actor
Monk
Sorcerer
Cultist"
  # Descriptor ----
    descriptor <- "Dependable
Gentle
Suspicious
Intelligent
Talented
Artistic
Courageous
Joyful
Industrious
Foolhardy
Brooding
Dim
Talkative
Enigmatic
Confused
Anxious
Impulsive
Ambitious
Frightened
Egotistical
Deluded
Bad-tempered
Bossy
Monomaniacal
Aggressive
Devious
Insane
Psychopathic
Bloodthirsty
Practical
Sympathetic
Introverted
Extroverted"
  # Result ----
    raw <- strsplit(x = descriptor, split = '\n')[[1]]
    res <- sample(raw, 1)
    raw <- strsplit(x = occupation, split = '\n')[[1]]
    paste(res, sample(raw, 1))
}

#' @export
handbook_clue <- function(){
  # Wall of text ----
  raw <- "A strange, gooey substance that gives off a bizarre, unidentifiable smell.
A last will and testament in a sealed envelope.
A map of a place you have never seen before. One location on it has a handwritten arrow with the word \"Here\" next to it.
Some sheets of paper filled with indecipherable symbols.
A key, engraved with strange glyphs.
A ripped piece of clothing, bloodstained
A severed limb
A severed extremity
A severed head
Strange odour on the air
A mysterious figurine
An empty, but ornate jewellery box
A child's doll
A perfectly spherical stone
A strange coin
An urn of ashes
Belongings, supplies, all torn and ripped open. Possibly a backpack which looks like it has been rifled through by a beast of some kind.
A strange looking hair / in the wild, a tuft of fur caught on a branch.
A note left by a professor friend.
Someone is singing a strange song. You might meet them in the open, or you might hear them in the room next door. The song haunts you.
A rusty old carving knife with nicks in the blade.
A discarded revolver
Shell casings from a shotgun
Rope, obviously cut with a blade
Tracks leading away from this area
Blood stains spattered up the wall / on a nearby boulder or tree if outside
A rune, made out in carefully placed stones on the ground
A dead body bearing suspicious markings - whippings, brandings?
Car tyre tracks
A note with only the name of the nearby town written on it
A rope dangling from above
A grave which has been dug out, the shovel left behind.
A broken lantern
A bear or man trap spattered in blood
Singing, distant and mournful
A clear trail where something large has been dragged away
A chill wind, no matter what the weather
A map of a labyrinth neatly made on a piece of paper
A letter of recommendation from an aristocrat
The sound of glass smashing
Gunshots in the distance!
A piece of shell that looks like it came from a large egg
Book containing a history of the world - but certain events have been crossed out messily in black pen, and the word WRONG scrawled around them.
An empty liquor bottle
An old wooden toy you used to play with as a child and had forgotten until now.
Clothing, ripped and torn
A shattered timepiece
A timepiece that runs backwards.
A ancient relic of some sort
A grimoire or Mythos tome"
  # Processing ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_effect_auditory <- function(){
  # Effect ----
  effect <- "Creaking
Whispering
Dripping
Chanting
Singing
Groaning
Moaning
Screaming
Indistinct talking
Laughing
Scraping
Rattling
Hissing
Watery sounds
Jolting impacts
Thudding / thumping
Footsteps
Breathing
Burrowing
Growling
Slippery slopping
Knocking
Buzzing
Rumbling
Crying"
  # Location ----
  location <- "Under floor/ground
In wall
From outside
From everywhere
Indeterminable
Inside your head?
Inside an object
Upstairs / Downstairs
From the water which is slowly filling up the room.
From the next room
From somewhere distant
From within an object
Right in your ear"
  # Result ----
  raw <- strsplit(x = effect, split = '\n')[[1]]
  res <- sample(raw, 1)
  raw <- strsplit(x = location, split = '\n')[[1]]
  paste(res, sample(raw, 1))
}

#' @export
handbook_effect_visual <- function(){
  # text ----
 raw <- 'Strange lights coming from outside
The dark silhouette of a person in the room / nearby
Water begins seeping in under the door
A child, standing, simply staring at you
A face at the window, just for a moment
The shape of a person (or creature) walking past the window
A wounded animal, lying on the ground, whimpering
The power goes out
Blood and guts strewn over the floor.
Shadows on the wall, vast and looming.
Bugs crawling over every surface.
Pools of blood begin collecting on the floor.
Blood seeping from the walls
On its own, the radio turns on and starts playing jazz.
One or more dead bodies
Your teeth begin coming loose, falling out of your mouth
A giant hole, an abyss, opens in the floor in front of you
The patterned wallpaper starts moving (or if outside) you see faces appear in the trees, rocks, everywhere, moving
Everything goes dark, and an ominous voice addresses you
Ghostly figures moving about
The walls are breathing, pulsing slowly in and out
A vision of yourself being brutally killed
Writhing tentacles, out of nowhere, wrap you in their embrace
The furniture becomes a living thing, made of flesh, with pulsating veins.
The shape of a Mythos creature approaches... Is it real?'
 # result ----
 raw <- strsplit(x = raw, split = '\n')[[1]]
 sample(raw, 1)
}

#' @export
handbook_rumour <- function(){
  # text ----
  raw <- "A local mob boss has just been nabbed by the cops.
That fire that started last week in the department store downtown? Apparently cops are scratching their head as to how it started. They found some strange corpses in the wreckage.
Local army veterans are talking about being approached by a shady outfit known as Black Tuesday for some work in South East Asia.
A local aristocratic family paid a hefty bribe for their son to be released from a mental asylum.
Flouting prohibition, a local distiller is peddling a whisky called Old Sea Dog. Apparently he adds some sort of sea creature for a very maritime flavour.
Some weird scientist guy has been scavenging a lot of parts from local scrap dealers. He keeps raving on about his Doomsday Machine.
A local manufacturer is getting a steady supply of slaves from somewhere for his factory.
The CEO of a local bank is using people funds for some truly bizarre purposes.
There are strange growths in the local forest, masses of vines that have completely engulfed whole areas of the wood.
The plans for the new town hall, apparently the structure will be unlike anything this town has ever seen before. The architect is a real eccentric, it's said.
The army has taken over a family's house on the outskirts of town. It it completely cordoned off. No-one knows why, and the family hasn't been seen for weeks.
The mayor of the next city over has been dead for years, apparently. Some sort of clone has taken her place.
A bunch of local thugs has become significantly more violent of late. They appear under the influence of drugs of some sort.
A storyteller who is passing through town is said to be attracting huge audiences with his weird tales, which leave the recipients... changed.
Well known explorer Sir George Head has just returned from Antarctica, stark raving mad.
Several people in your town have just been arrested for identity theft.
You know Luigi's pizzas, downtown? If you order the mushroom and pepperoni, you'll be seeing some wild shit!
There's supposed to be a secret train/subway stop somewhere on the line.
  Several workers off a ship recently docked in harbour are trying to claim asylum, saying the captain has lost his mind and has them working in slave-like conditions.
  Whatever happens, make sure you don't get checked into the local hospital. Something weird is going on there.
Everyone knows that Edward Perkins the butcher secretly mixes human remains into his award-winning sausages.
If you want to join the local triad, you have to first find a gift for their boss.
In tunnels deep, deep beneath the city, it's rumoured that scientists are carrying out some bizarre experiments on life-forms they've found down there.
A new drug has emerged on the black market, more powerful than anything known.
I hear the tavern known as The Sailor's Arms is a good place to pick up rumours and get hooked up in other ways too.
You hear that a local councillor is not even human.
There's a secret train/subway stop somewhere on the line. The trick is finding it.
There's an underground fighting ring being operated somewhere.
Someone has been spiking drinks at the local tavern.
If you need some quick cash, the local cops are hiring private investigators to look into the Purple Lotus murders.
You hear that a local corporation has been involved in human trafficking
There's been a lot of weather balloons hovering over the city. At least that's what people think they are.
Insects have been infesting the crops of local farmers. Some say there hasn't been an infestation this bad since the summer of 1872.
The old statue of the city's founder (in the square) has been dripping blood from its eyes.
Homeless people living in the sewers are saying they're seeing strange creatures down there.
A coworker tells you, You know that guy who died a few years back? I could have sworn I saw him in the street today!
You hear a rumour about a strange relic that has surfaced.
Screams can be heard from the abandoned house on Whittaker Ave.
There's good money being paid for fresh human organs down at the local medical practice.
A restaurant just recently opened in the downtown area is serving up some truly original dishes.
A cat has been standing vigil at the tomb of old widow Saunders in the local cemetery. When approached, it hisses and attacks anyone who tries to disturb it or move it.
The town council has just sent out a notice saying that the four blocks from Thomson Street down to Atkins Street will be closed this Saturday, but they're not giving details as to why.
A charismatic figure has appeared and has set up a commune just out of town. He is attracting a lot of followers.
Have you seen that weird pair that walk through town every afternoon at 4:04? It's an old man in a top hat and a lady (apparently) whose face is entirely hidden by a veil.
A local boy has been accused of devil worship, but he is proclaiming innocence, saying he doesn't know how those weird books wound up in his school desk.
Dead vermin and birds have been littering the downtown streets. No one knows why.
A local corner preacher is proving an interesting attraction. Apparently a lot of his predictions are eerily close to the mark.
Several people have come forward to the local cops with a disturbing tale about waking up to find a lady watching them sleep from outside their bedroom window. When they go to check, he's gone.
You hear rumours that a local priest (by all accounts a devout Christian) has been dabbling in the occult."
  # result ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_random_event <- function(){
  # text ----
  raw <- "Make a Spot Hidden roll, standard. If successful, you notice that someone is following you. You don't give anything away just yet, but they've been shadowing you for several days. What are their motives? Could it be to do with what you've been investigating recently?
A friend of yours, a professor, gets in touch with you regarding a disturbing discovery he has made. He's heard what you've been researching and has a warning.
Late this night, when you go to bed, you hear a swish. Turning on the light, you see that a note has been slipped under your door. Unfolding it, you read the contents, which make your blood run cold... (interpret according to campaign).
A nearby telephone rings. You feel an impulse to pick it up. You do, but at the other end all you hear is faint talking, as if the other person is many many miles away. It is whispering numbers.... co- ordinates?
You meet another investigator, who coincidentally has been on the same trail as you and has some discoveries that she would like to share. How is it that you are both investigating the same thing? Anyway, no matter, the knowledge she has is crucial.
You notice a learned-looking person watching you. Do you approach them? Their expression is one of fear mixed with curiosity, anxiety... as if they are burning to tell you some terrifying fact!
In a much-loved book, you discover a bookmark that you didn't think you had before. It bears a strange symbol on it, which you can't make sense of.
Reaching into your jacket pocket, you find (insert party number here) one way tickets to (insert exotic destination here), along with a note: Don't ask any questions. Just be on the boat. Trust me.
An envelope arrives, addressed to you. Inside is a movie ticket, and a note: Roxy Theatre, 11pm showing, The Tides of Fate, Row F, Seat 23. What could this mean?
A persistent and foul odour is coming from somewhere... beneath your house? At least that's where you think it's coming from.
A reporter has heard about your investigation and begins hounding you, asking all sorts of questions.
A package arrives at your address. Inside is a dictionary, English to (insert made up or unknown language here). There is no sender address, nothing to indicate where it came from. You begin researching where this language comes from.
You find a bunch of notes, or they are mailed to you. What is the nature of them? Refer to random verb table for direction. They could be a few hastily-scrawled passages from a Mythos tome.
You find an old tattered coat (on your landing, blowing up the street, stuck on a bush). Inspecting it, you find a note in one of its pockets.
In the news, you read that a meteorite has landed near your town. Curious, you begin making inquiries but after a day you realize that all mention of it has been erased, covered over, hushed up. Why?
Investigators harassed by suspicious cop, possibly related to mystery or just an asshole cop.
You meet someone who shares a vital piece of information regarding the quest.
You find a dropped purse or wallet. The contents are intriguing to say the least.
You find a map, discarded, or just a fragment of one. What you see arouses your curiosity.
In the hallway outside your apartment, or around your house, you notice seaweed. In fact, you begin noticing seaweed wherever you go. What is leaving this here or causing it to appear?
You notice that a mechanical hum has started coming from your basement. It starts at the same time every night, about 2am, and finishes at 5am. Will you go and investigate? What could it be?
A crow perches on your windowsill at the same time every day. It croaks strangely. After some time you realize that the crow is in fact talking, and it repeats the same word every time...
You witness a crime and have a chance to stop the criminal. If you do, you may win the favour of the local constabulary, who might be able to help you out. Or is the crime some sort of trap engineered to lure you in?
You see someone acting suspiciously. When they see you, they turn tail and run!
A con-man has set up a table in the main street and is selling some truly bizarre items. He seems in a hurry to get rid of them as quickly as possible.
An old man comes out of a brick house, dumps some rubbish into a bin, kicks a cat and walks down the road. A minute later, an old man wearing the exact same clothes, with the exact same gait, and the exact same walking cane, dumps some rubbish into the bin, kicks a cat and walks down the road the same direction.
Your neighbour/the person in the next hotel room is acting very weird. They seem high on drugs all the time and incredibly paranoid. They also have some weird OCD habits which are causing you to wonder whether they are entirely sane.
Your dreams are haunted every night by a weird, rhythmic chanting in an unknown tongue. When you wake you can remember the words vividly, even though you have no idea what they mean.
You meet another investigator who appears to be on the same trail as you. (Perhaps they phone you or turn up at your door). Are they trustworthy, or are they not all that they seem?
There is a knocking at your door, but when you answer, no-one is there. You shut the door and head back inside. Ten minutes later, the knocking again... (In other situations - eg outdoors - perhaps you hear someone calling your name, but when you investigate, you can't find them).
While exploring a building, you discover a secret door. What secrets lie behind?
A street dog runs past with a human hand in its mouth, the ring finger bearing a large, jewelled ring.
You notice a house on the next street over from yours which is all boarded up, the doors locked, the gate padlocked. A passerby informs you that this is the house of an aristocrat who hasn't been seen in years.
All the rats have come flooding out of the sewers in droves. They appear absolutely manic, and have overrun the town. Something has scared them out of their home, perhaps...
An NPC who has been helping you out has disappeared. Making Inquiries, you can find no trace of him, no history, nothing. It is as if he never even existed.
Someone you have been following or investigating is arrested and detained for an unrelated crime. Or is it unrelated?
Your pet (if you have one) suddenly starts growling at you, showing its teeth. Previously this animal loved you, now it seems that the creature is repelled by you.
Your cat starts to hiss at the wall.
Bugs begin massing on the walls in your apartment, but often they make strange patterns, like symbols.
At the lights, you notice a running car, but no-one is at the wheel. In the backseat, perhaps, is a passenger, dead.
While walking somewhere, or riding a train perhaps, you notice a woman slumped over the book she is reading. You watch for some time - she doesn't appear asleep. Concerned, you go over and tap her on the shoulder. Her head lolls to one side, and you realize she is dead! Shocked, you happen to catch sight of the book - it is unlike anything you have seen, full of strange sigils, runes and disturbing images (a Mythos book).
As you are walking, a large projectile lands beside you heavily, just missing you. You look up - did someone drop this on you from above, trying to kill you? If you are indoors, perhaps it was a falling chandelier or some other domestic item.
Something happens that requires you to make a skill roll! Have a look at the Skills list on your Investigator sheet and figure out what it might be in the context of your story.
Stormy weather closes in, thunder and lightning. When the lightning cracks, it illuminates the shape of a colossal, heart- stopping beast. Is that real, or are you merely hallucinating?
You are overcome by terrifying thoughts. Awful visions of eldritch creatures who you can barely stand to imagine.
The local police approach you, saying that you fit the description of someone who just killed their entire family and fled.
You receive notification of the death of a relative or close friend.
One of your friends has been kidnapped. Investigating the place where they were abducted, you find signs of a struggle.
You find graffiti which might seem nonsensical to a passer-by, but
makes terrible sense to you. The message will be a timely warning 97 and will be directly relevant to the quest at hand. Someone like you has been here before, in the same situation as you, and has left their
thoughts here. Beware the _____, watch out for their _____ or something similar.
You are attacked!
A Mythos creature appears, terrifying to behold. (Use Q/A rolls to determine the nature of the beast and its current disposition). Also, if it makes no sense for it to be there, then perhaps it is an hallucination."
  # result ----
  raw <- strsplit(x = raw, split = '\n')[[1]]
  sample(raw, 1)
}

#' @export
handbook_verb <- function(n = 1){
  toolbox_verb(n)
}