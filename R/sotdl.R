curve_3d6 <- function(x){
  c(x[1],
    rep(x[2], times = 2),
    rep(x[3], times = 3),
    rep(x[4], times = 4),
    rep(x[5], times = 3),
    rep(x[6], times = 2),
    x[7])
}

#' @export
sotdl_book <- function(){
  # Form ----
  form_roll <- roll(3,6)
  form_df <- data.frame(num = c(4, 8, 12, 15, 17, 18),
                        form = c(paste('Tablet made from',
                                     sample(c('clay or wax',
                                              'wood',
                                              'stone',
                                              'scale of a large reptile',
                                              'chitin',
                                              'bone'),
                                            1)),
                                 paste('Scroll made from',
                                       sample(c('papyrus',
                                         rep('parchment or vellum', times = 2),
                                         rep('paper', times = 2),
                                         'snakeskin, scales, living skin'), 1)),
                                 'Folio with about 25 pages',
                                 'Book with about 100 pages',
                                 'Tome with about 200 pages',
                                 'Codex with 500 or more pages'),
                        stringsAsFactors = F)
  form <- form_df$form[which(form_roll <=form_df$num)[1]]

  # Cover ----
  cover <- sample(c('No cover (never had one / lost)',
                    'Thin wood',
                    'Thin wood wrapped in cloth',
                    'Thin wood wrapped in leather',
                    'Thin wood wrapped in skin',
                    'Thin wood or metal covered in gold leaf',
                    'Bone, ivory, horn or teeth',
                    rep('Rusted iron, steel, bronze or gold', times = 3),
                    'Wood and covered in spikes',
                    'Metal that bristels with blades and barbs',
                    'Living tissue, possibly a face or faces'),
                  size= 1)
  cover <- ifelse(roll(3,6) == 16,
                  yes = paste0(cover, ', locked (1/6 also has key)'),
                  no = cover)
  cover <- ifelse(roll(3,6) == 17,
                  yes = paste0(cover, ', wrapped in chains or razor wire'),
                  no = cover)
  cover <- ifelse(roll(3,6) == 18,
                  yes = paste0(cover, ', animated organ affixed to surface'),
                  no = cover)
  # Pages ----
  pages <- sample(curve_3d6(c('Hair, tissue, nails',
                              'Parchment',
                              'Vellum',
                              'Paper',
                              'Onionskin',
                              'Metal plates',
                              'Glass')),
                  size = 1)
  # Condition ----
  cond_roll <- roll(3,6)
  cond_df <- data.frame(num = c(5, 8, 12, 15, 18),
                        cond = c('Considerably damaged with most pages eaten by worms, burned, or missing. Water damage might make most of it illegible, too.',
                                 'Some light use',
                                 'Worn with several bent or torn pages, scratches, tears or stains on the cover',
                                 'Poor shape with loose pages, extensive staining and possible burns',
                                 'New or appears new'),
                        stringsAsFactors = F)
  condition <- cond_df$cond[which(cond_roll <=cond_df$num)[1]]
  # Authors ----
  nauth <- ifelse(test = sample(1:3, 1) == 1,
                  yes = roll(1,3) +1,
                  no = 1)
  # Script ----
  script_raw <- curve_3d6(c('crazed',
                            'scrawled, messy',
                            'small, crampled',
                            'printed',
                            'flowing',
                            'big, bold',
                            'illuminated'))
  scripts <- sample(script_raw, size = nauth)

  # Ink ----
  ink_raw <- curve_3d6(c('bodily fluids such as blood or urine',
                       'squid ink, lemon juice',
                       'oil-based in black, blue or brown',
                       'water-based in black, blue or brown',
                       'painted',
                       'silver, gold, etched or scratched',
                       'poisonous'))
  inks <- sample(ink_raw, size = nauth)

  # Author string ----
  auths <-paste0('Author ', 1,
  ': ', scripts[1],
  ' script and ',
  inks[1], ' ink.')
  if(nauth > 1){
    for(i in 2:nauth){
      auths <- paste0(auths,'\n','Author ', i,
                      ': ', scripts[i],
                      ' script and ',
                      inks[i], ' ink.')
    }
  }
  # Illustrations ----
  illustrations <- sample(curve_3d6(c('Abundant',
                                    'Some',
                                    'Sparse',
                                    'None',
                                    'Sparse',
                                    'Some',
                                    'Abundant')),
                          size = 1)
  illustration_quality <- sample(curve_3d6(c('Disgusting',
                                           'Ugly',
                                           'Varied in quality, some fine others not',
                                           'Crude or simple',
                                           'Detailed',
                                           'Beautiful',
                                           'Animated')),
                                 size = 1)
  # Contents ----
  content <- sample(c('False information',
                      'Architecture or engineering',
                      'Astrology or Folklore',
                      'Etiquette & Customs or Heraldry',
                      'Geography or Navigation',
                      'History or Literature',
                      'Law or Politics',
                      'Magic or Occult',
                      'Medicine or Nature or Science',
                      'Work of fiction',
                      'A diary or journal',
                      'A screed or libel against a person, organization or nation',
                      'Pornographic',
                      'Gibberish or blank',
                      'Philosophy or religion',
                      'War',
                      rep('Incantations', times= 4)),
               1)
  # Special Features ----
  special_roll <- roll(1,6)
  if(special_roll == 1){
    special_stuff <- c('Stains the soul of readers',
                       rep('Unreadable unless in a mirror, moonlight or darkness',
                           times = 2),
                       rep('Weird binding, pages held by worms, tiny clutching hands or teeth',
                           times = 3),
                       rep(paste('Secret compartment containing',
                                 sample(c('Poisonous gas',
                                          'A letter',
                                          'Nothing',
                                          'Nothing',
                                          'Money',
                                          'An enchanted object'),
                                        size = 1))),
                       rep('Inspires madness and obsession', times = 2),
                       rep('Infested with toothy parasites', times = 2),
                       'Magical trap, reading any words causes it to explode',
                       'Is alive, has teeth and will bite')
    n_special <- roll(1,6) - 3
    if(n_special < 1) n_special <- 1
    special <- paste(sample(special_stuff, size = n_special),
                     collapse = ';')
  }
  # Incantation ----
  if(content == 'Incantations'){
    inc_n <- sample(c(1, roll(1,3), roll(1,6), roll(2,6)), size = 1)
    incantations <- data.frame(tradition = replicate(rand_magic_tradition(),
                                                     n = inc_n),
                               level = sample(curve_3d6(c('4',
                                                          '3',
                                                          '1',
                                                          '0',
                                                          '2',
                                                          '5',
                                                          '6+')),
                                              size = inc_n))
  }
  # Final result ----
  res <- paste0('Form: ', form, '\n',
                ifelse(form_roll %in% 5:8,
                       yes = paste0('Case: '
                                    , cover,
                                    '\n'),
                       no = ''),
                ifelse(form_roll > 8,
                       yes = paste0('Cover: ',
                                    cover,
                                    '\n'),
                       no = ''),
                ifelse(form_roll > 8,
                       yes = paste0('Pages: ',
                                    pages,
                                    '\n'),
                       no = ''),
                'Condition: ', condition, '\n',
                auths, '\n',
                'Illustrations: ', illustrations,
                ifelse(illustrations != 'None',
                       yes = paste0(' (',
                                    illustration_quality,
                                    ') \n'),
                       no = '\n'),
                'Contents: ', content, '\n',
                ifelse(special_roll == 1,
                       yes = paste0('Special features: ', special, '\n'),
                       no = '')
  )
  cat(res)
  if(content == 'Incantations'){
    print(incantations)
  }
}
