# Functions to create a random settlement
# Sources:
#  Settlement rules, PFRPG
#  Empire Builder series, Ennead Games

#' @export
settlement_size <- function(){
  sizes <- c("Thorp (<21)",
             "Hamlet (21-60)",
             "Village (61-200)",
             "Small town (201-2000)",
             "Large town (2001-5000)",
             "Small city (5001-10000)",
             "Large city (10001-25000)",
             "Metropolis (>25000)")
 res <- min(sample(1:8, size = 2, replace = T))
 sizes[res]
}

#' @export
settlement_name <- function(){
  option <- as.logical(rbinom(n = 1, size = 1, prob = 0.5))
  prefix <- c("Castle", "Cape", "Deep", "East", "Fort",
              "Grand", "Greater", "High", "Higher", "Inner",
              "Lake", "Lesser", "Long", "Low", "Lower",
              "Middle", "Mount", "New", "North", "Old",
              "Outer", "Port", "South", "Upper", "West")
  res <- paste0(sample(settlement_names_main, 1),
                sample(settlement_names_suffix, 1))
  ifelse(option, yes = paste(sample(prefix, 1), res), no = res)
}

#' @export
settlement_government <- function(){
govs_5e <- c("Autocracy","Bureaucracy","Confederacy","Democracy","Dictatorship",
             "Feudalism","Gerontocracy","Hierarchy","Magocracy","Matriarchy",
             "Militocracy","Monarchy","Oligarchy","Patriarchy","Meritocracy",
             "Plutocracy","Republic","Satrapy", "Kleptocracy","Theocracy")
sample(govs_5e, 1)
}

#' @export
settlement_location <- function(){
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