#' @export
rand_gma <- function(){
  r1 <- c("rearrange","trap","replace","degrade","defenestrate","bless","fix","wish","eliminate","inflict",
          "prevent","antagonize","bargain","judge","pardon","convince","meet","return","unearth","take",
          "absolve","defeat","alter","reward","trick","release","distract","weaken","punish","regenerate",
          "exhaust","restore","bestow","interrupt","explore","steal","beguile","halt","discuss","confuse",
          "travel","remember","forget","avenge","strengthen","delay","detect","deactivate","decrease","disturb",
          "enslave","hide","allow","deviate","deny","move","join","exchange","discover","pacify")
  r2 <- c("helpful","graceful","fraudulent","drained","sacreligious","stealthy","distant","temporary","traumatic",
          "wonderous","broken","ancient","arrogant","hopeful","benevolent","complex","impressive","irksome",
          "hallucinatory","disruptive","psychological","gaudy","diabolical","frightful","ghostly","unlikely",
          "elusive","impending","defiant","despondent","advanced","physical","metaphorical","material",
          "aberrant","fair","restrained","eternal","discuss","arcane","unclear","repulsive","minor","feral",
          "disappointing","tender","treacherous","amicable","scientific","precise","personal","ambitious","angelic",
          "immaculate","active","artificial","bewildering","religious","widespread","mental")
  r3 <- c("solution","pandemic","wilderness","freedom","investment","ward","rage","leader","purity","companion",
          "tension","relative","aura","haven","daemon","merchant","magic","skill","conflict","poverty","civilization",
          "defense","trap","source","grudge","wreckage","locals","rebellion","illness","superstition","parent","foe",
          "temple","tool","ability","monument","lookout","underling","goodness","container","training","stockpile",
          "knowledge","pain","food","game","deity","death","treasure","barrier","chaos","structure","trial","vehicle",
          "dream","time","prophecy","fear","weapon","font")
  return(paste(sample(r1,1),sample(r2,1),sample(r3,1)))
}

#' @export
rand_city_location <- function(){
  r <- c("Abbey","Altar","Arch","Catacomb","Cemetery","Church","Coliseum","Crypt","Exorcist",
"Exposure tower","Fountain","Missionary society","Monastery","Nunnery","Oracle","Parade grounds",
"Park","Public square","Religious School","Ruins","Shrine","Statue (spire, stele)","Temple","Theatre",
"Archives","Asylum","Barracks","Baths","Bureaucrat","Cavalry stable","Censor","Civil court","Clinic",
"Criminal court","Customs","Executioner","Fountain","Guard headquarters","Hospital","Jail","Library",
"Mayor's home","Meeting hall","Mint","Monolith","Monument","Palace","Park","Prison","Punishment square",
"School","Sherrif","Tax collector","Toll collector","Town hall","Treasury","University","Watch tower",
"Ambassador","Witch","Wizard","Bakers","Barbers","Bathers","Beer Sellers","Blacksmiths","Bleachers",
"Buckle Makers","Carpenters","Chandlers","Chicken Butchers","Coopers","Copyists","Cutlers","Doctors",
"Fish Merchants","Fletchers","Furriers","Glove Makers","Harness Makers","Hatmakers","Hay Merchants","Innkeepers",
"Jewelers","Laundresses","Locksmiths","Maidservants","Manservants","Masons","Meat Butchers","Mercers",
"Oil Merchants","Old-Clothes Dealers","Painters","Pastrycooks","Plasterers","Porters","Purse Makers",
"Restauranteurs","Roofers","Ropemakers","Rugmakers","Saddlers","Scabbard Makers","Sculptors","Shoemakers",
"Spice Merchants","Tailors","Tanners","Water Carriers","Weavers","Wine Sellers","Wood Sellers","Woodcarvers",
"Architect","Bricklayer","Carpenters workshop","Foundry","Furnishings store","Glassblower","Iron casting",
"Laborer's hall","Lumberyard","Property management","Real estate broker","Rentals","Smelter","Stone mason",
"Tile maker","Armorer","Assassin","Catacomb","Cemetery","Crypt","Duel challenger","Embalmer","exposure tower",
"exterminator","fletcher","morgue","Sword for hire","Swordsmith","Taxidermist","Tombstone cutters","Alchemist","
Aquarium","Arboretum","Artist's studio","Astrologer","Bookseller","Boxing & wrestling academy","Calligrapher",
"Cartographer","Dancing academy","Fencing academy","Finishing school","Fortuneteller","Genealogist",
"Historical society","Kindergarden","Law offices","Library","Magic academy","Mesmerist","Museum",
"Natural curiosity collector","Newspaper","Numerologist","Observatory","Oracle","Paper maker",
"Philosophical academy","Portrait artist","Private investigator","Sage",
"School (primary, secondary, boarding, private, racial)","Scribe","Secret societies","Training school",
"University","Ampitheater","Aquarium","Arena","Athletic Club","Banquet Hall","Beach","Brothel","Carnival",
"Casino","Circus","Coliseum","Fairground","Gynasium","Inn","Museum","Music Hall","Park","Pool","Private Club",
"Race Track","Sport Fields","Stadium","Theater","Theatre","Tourement Field","Zoo","Barber","Costume shop",
"Fabric merchant","lorist","Fur merchant","Gentlemen's clothes","Hat shop","Ladies clothes","Leather worker",
"Outfitters","Rag shop","Shoe maker","Tailor","Tattoo and dye artist","Textiles manufacturing","Wig shop",
"Armed carriage (money trasport)","Auction house","Bank","Casino","Coin collector","Collection agency",
"Criminal organization","Fence","Insurance issuer","Law offices","Merchant bank","Merchant's office","Money changer",
"Money lender","Pawn shop","Property management","Real estate broker","Stock market (bourse, exchange)",
"Tax collector","Ale","Animal pens","Baker","Bakery","Banquet hall","Beer","Brewer","Brewery","Butcher","Cafe",
"Cafeteria","Cheese","Dairy","Dairy goods","Farm","Farmers market","Fine food & drink","Fish","Fishmonger",
"Foodstuffs","Fresh food","Garden (surface & rooftop)","General food & drink","Grainery (grain silo)","Greenhouse",
"Greengrocer","Grist mill","Grog shop","Ice shop","Inn","Liquor","Luxury food store","Mead shop","Meats/Butchery",
"Mill","Mushroom (& root vegetables) seller","Nursery","Opium den","Orchard","Restaurant","Slaughterhouse",
"Smoke house","Spice merchant","Sweet shop","Tavern","Tobacconist","Wine merchant","Administrative office",
"Armory","Barracks","Castle","Court","Embassy","Fire station","Fletcher","Gate","Guard tower","Herald/ messengers",
"Library","Naval yard","Orphanage","parade grounds","Police station (constables, thief takers)","Poorhouse",
"Prison","Refuse collector","Road","School","Tax collector","Towers","Town walls","Workhouse","Antique emporium",
"Auction house","Bone and shell scrimner","Clock smith","Coin collector","Coppersmith","Florist","Gemcutter",
"Goldsmith","Jewellers","Luxury food store","Objects d'art","Whitesmith (platinum and silver)","Abortionist",
"Apothecary","Barber","Folk remedies","Hospital","Midwife","Opium den","Physician","Surgeon","Beach","Cemetery",
"Ceremonial square","Farm","Fields","Garden","Orchard","Parade grounds","Park","Public square","Vacant lot",
"Apartment","Barracks","Beggar's alley","Boarding House","Castle","Citadel","Club","Embassy","Estate",
"Flop house","Garden","Greathouse with grounds","Home","Homeless encampment","Hostel","House","Hovel","Hut",
"Icehouse","Inn","Lighthouse","Lodging house","Mansion","Monastery","Nunnery","Orphanage","Palace","Poorhouse",
"Prison","Private club","Property management","Rental house","Shelter","Slum","Tavern","Townhouse","workhouse",
"Animal Feed","Antiques","Apothecary","Apothocary","Arborist","Armorer","Artworks","Bait & Tackle","Baker",
"Barber","Blacksmith","Books","Bowyer/Fletcher","Brass Founder","Brewer","Bricklayer","Brickmaker","Brooms",
"Brothel","Butcher","Candlemaker","Candles","Carnal House","Carpenter","Cartwright","Ceramics","Chandler",
"Charcoal","Cheese Maker","Cloth","Clothier","Coins","Cooper","Cosmetics","Costumes","Curiosity Shop","Dice",
"Dyer","Farrier","Firewood","Flowers","Fuel Store: (Coal, Peat, Kindling, Oil)","Fuller","Furniture","Furrier",
"Furs","Games","Gem Cutter","Gifts","Glass","Glassblower","Glue Factory","Goldsmith","Greenhouse","Grocer",
"Groom","Guild Hall","Haberdasher","Hardware/Tools","Harness Maker","Herbalist","Herbs","Imported Goods","Ink"
,"Inn","Jeweler","Jewlery","Laundry","Law Offices","Leather Goods","Leatherer","Lens Grinder","Linen","Livestock",
"Locksmith","Magic Items/Charms","Maps","Mason","Mechanist","Miller","Miniatures/Figurines","Mirrors","Money Changer",
"Musical Instruments","Nets","Notary","Novelties","Nursery","Oil Merchant","Painter/Limner","Paper",
"Paper/Parchment Maker","Pawn Broker","Perfumes","Pets/Familiars","Pipes/Tobacco","Potions","Potter","Poulterer",
"Rare Woods","Religious Items","Rendering Plant","Rope","Rope Maker","Rugs And Tapestries","Rugs/Tapestries",
"Saddler","Saddles","Scribe","Scrivener","Servant Referral (Nannies, Cooks, Gardeners, Butlers Etc)",
"Ships Supplies","Shoemaker","Sign Painter","Silversmith","Slave Auction Blocks","Slave Catcher","Slaves","Soap",
"Soap Seller","Spell Emporium","Spices","Stable","Stonecutter","Sundials","Sweets/Candy","Tailor","Tanner",
"Tannery","Tavern","Temple","Tiler","Tinsmith","Toy Store","Toys","Trinkets/Curios","Warehouse","Weaponsmith",
"Weaver","Wheelwright","Whips","Wigs","Wine Merchant/Vintner","Wool","Alchemist","Apothecary","Architect",
"Armourer (all types)","Armourer (leathers & hides)","Artificer/Mechanician","Artist","Astrologer","Baker",
"Barber","Basketmaker","Beader","Bellfounder","Blacksmith","Bladesmith (knife & dagger blades)","Bonecarver",
"Bookbinder","Bookseller","Bottlemaker","Bowyer (composite bows)","Bowyer (normal bows)","Boxmaker",
"Brazier (brass worker)","Brewer","Bricker","Builder","Butcher","Cabinetmaker","Cardmaker (playing cards)",
"Carpenter","Cartmaker","Cartwright","Carver","Chandler","Chandler (wax & tallow candles)",
"Chapemaker (buckles & scabbard fittings)","Cheesemonger","Clothier","Coachmaker","Cobbler",
"Cobbler/Shoemaker","Cofferer","Coffinmaker","Cook","Cooper","Coppersmith","Cordwainer (shoes & boots)",
"Coursours (horse dealer)","Crossbowyer","Currier (leatherworker)","Cutler","Dairy","Distiller","Draper",
"Dyer","Embroiderer","Engineer","Exterminator","Farrier","Felmonger (un-tanned skins)","Feltmaker",
"Fine metal worker","Fishmonger","Fletcher (arrows & darts)","Founder (casts iron)",
"Fruiterer","Fuller","Furbisher (cleaner & polisher)","Furniture carver","Furrier","Fuster (woodworker)",
"Garbler (spice sifter)","Gemcutter","Girdler (belts)","Glasier","Glassblower","Glover","Goldsmith",
"Greengrocer","Grocer","Guide/Messenger","Haberdasher","Haberdasher/Hatter","Harnessmaker","Hatter",
"Hawker","Heaumer (helms)","Herbalist","Hosier","Illuminator (manuscripts)","Inker","Instrumentmaker (musical)",
"Instrumentmaker (scientific)","Ironmonger","Jeweller","Joiner","Lantern-maker",
"Latener (late = brass like material)","Lauderer","Leather worker","Leatherworker",
"Limner (insignia & portraits)","Limner/Painter","Locksmith","Loriner (metal saddle fittings)",
"Macer (maces, flails, morningstars)","Marbler","Mason","Material shop","Mercer (silks, lace etc)",
"Metal worker","Miller","Moneylender/Banker","Moneylender","Nailor (nails)","Needler","Netmaker",
"Oilpresser","Pack & Pouch maker","Painter","Parchmenter","Pasteler","Pavior","Pawnshop","Pepperer",
"Pewterer","Pickler","Pinner (pins)","Plasterer","Plumber","Pole Armourer","Porcilinist","Potter",
"Poulterer","Quarreller","Quivermaker","Roofer","Ropemaker","Sacker","Saddler","Sage","Sailmaker",
"Salter","Sawyer","Scabbardmaker","Scrivener","Sculptor","Seal maker","Ship's Chandler","Shipwright",
"Shopper Services (10% fee will collect goods)","Silversmith","Skinner","Spurrier","Stables","Stainer",
"Stringer","Stuffer (furniture stuffer)","Swordsmith","Tablemaker","Tailor","Tanner","Tapicer (tapestry)",
"Taxidermist","Thacther","Tiler (roofing)","Tinderboxman","Tinker","Tinner","Traveler's Supplies","Turner",
"Upholder (household knick-knacks)","Upholsterer","Vintner","Wainwright",
"Weaponsmith (axes, javelins, picks, spears & slings)","Weaver","Weaver (linen)","Weaver (wool)",
"Wheelwright","Whitesmith","Wine Shop","Wineskinner","Wiredrawer","Wiremonger","Woodcarver",
"Wooler (woolen clothing)","Armor","Armor repair","Bowyer","Fine weapons","Fletcher","Sheilds","Weapons",
"Alchemists","Apothecaries","Armorers & weaponsmakers","Artificers","Artists","Assassins","Astrologers",
"Barristers","Blacksmiths","Caravaners","Carpenters","Coutesans","Entertainers","Fighters","Fishermen",
"Jewellers","Merchants","Mercinaries","Messengers/Heralds","Metalworkers","Moneylenders/changers","Physicians",
"Rangers","Sailors","Shipwrights","Slavers","Smugglers","Steersmen & navigators","Stonemasons","Tailors",
"Thieves","Wainwrights","Wizards","Animal trainer","Arbiter","Astrologer","Astronomer")
return(sample(r,1))
}

#' @export
rand_npc_attitude <- function(){
  r <- c("Alluring","Strong Presence","Aloof","Excitable","Argumentative","Pleasant","Arrogant","Leery",
         "Beautiful","Handsome","Boastful","Knowledgable","Boisterous","Un appable","Boring","Silly","Blunt",
         "Thin-Skinned","Cagey","Rude","Candid","Timid","Caustic","Guarded","Charming","Dramatic","Chauvinistic",
         "Ditzy","Clueless","Intimidating","Cold/Unemotional","Talkative","Confident","Plain","Conservative",
         "Humorous","Cordial","Evasive","Cranky","Fast Talking","Critical","Wallflower","Dainty","Oafish",
         "Dashing","Prim & Proper","Defensive","Snobby","Detached","Stubborn","Disrespectful","Popular","Dour",
         "Spontaneous","Down-to-Earth","Soft-Spoken","Droll","Merry","Dumb","Vivacious","Erudite","Morbid",
         "Extravagant","Polite","Fiery-Tempered","Meek","Freethinking","Unfriendly","Friendly","Nervous","Fussy",
         "Slick","Gregarious","Quiet","Glum","Personable","Humorless","Nice","Ignorant","Sympathetic","Impatient",
         "Shy","Important-Sounding","Quarrelsome","Inconsiderate","Perceptive","Interesting","Unhinged","Juvenile",
         "Quick-Witted","Likeable","Serious","Morose","Whimsical","Nonchalant/Cool","Sensitive","Obnoxious","Vulgar",
         "Socially-Skilled","Whiny")
  return(sample(r,1))
}

#' @export
rand_simple_oracle <- function(){
  return(sample(c("Yes, and",
                  "Yes",
                  "Yes, but",
                  "No, but",
                  "No",
                  "No, and"),
                1))
}

#' @export
rand_magic_tradition <- function(){
 r <- c("Air","Alchemy","Alteration","Arcana","Battle","Blood","Celestial","Chaos","Conjuration","Curse",
        "Death","Demonology","Destruction","Divination","Earth","Enchantment","Exorcism","Fey","Fire",
        "Forbidden","Illusion","Invocation","Life","Metal","Nature","Necromancy","Order","Primal",
        "Protection","Rune","Shadow","Shaman","Song","Soul","Spiritualism","Storm","Technomancy","Telekinesis",
        "Telepathy","Teleportation","Templar","Theurgy","Time","Transformation","Water","Witch")
 return(sample(r,1))
}

#' @export
rand_recluse <- function(pos = 1, neg = 1){
  white <- max(sample(1:6, pos, replace = T))
  black <- max(sample(1:6, neg, replace = T))
  if(white == black) return('Some presupposition behind the question is wrong!')
  if(white > black) res <- 'Yes'
  if(white < black) res <- 'No'
  if(white <=3 & black <= 3) res <- paste(res, ', but', sep = '')
  if(white >=4 & black >= 4) res <- paste(res, ', and', sep = '')
  return(res)
}

#' @export
rand_investigation_question <- function(){
  res <-  c("Yes, and a clue proves the entire questioned theory to be true.",
  "Yes. The answer is merely affirmative.",
  "Yes, but a clue proves some part of the questioned theory to be false.",
  "No, but a clue proves some part of the questioned theory to be true.",
  "No. The answer is merely negative.",
  "No, and a clue proves the entire questioned theory to be false.)")
  sample(res, 1)
}