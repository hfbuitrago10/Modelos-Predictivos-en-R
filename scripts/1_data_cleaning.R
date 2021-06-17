# Data cleaning
# Emilio Lehoucq

# Loading packages --------------------------------------------------------

library(tidyverse)

# Setting parameters ------------------------------------------------------

set.seed(607)

# Reading data ------------------------------------------------------------

load("unprocessed_data/data/35168-0001-Data.rda")

unprocessed_data <- da35168.0001 # N = 35,735, C = 802

# Keeping only complete observations on the responses ---------------------

processed_data <- unprocessed_data[complete.cases(unprocessed_data[ , 520:538]),] # N = 9,249, C = 802
# the max sample size for module A is 9,707, so no possible response missing more than 5% (at most 4.7%)

# Drop non-overlapping columns --------------------------------------------

processed_data <- processed_data[, colSums(is.na(processed_data)) != nrow(processed_data)] # C = 739

# Drop variables that don't make sense ------------------------------------

processed_data <- processed_data %>% 
  select(-c(CASEID, HRHHID, HRMONTH, HRYEAR4,HURESPLI,HUFINAL,HULANGCODE, HETENURE, HEHOUSUT,
            HETELHHD, HETELAVL, HEPHONEO, HWHHWGT, HRINTSTA, HRHTYPE, HRMIS, HUINTTYP,
            HUPRSCNT, HRLONGLK, HRHHID2, HWHHWTLN, HUBUSL1, HUBUSL2, HUBUSL3, GESTCEN,
            GCFIP, GCTCO, GTMETSTA, GTINDVPC, GCTCS, PERRP, PEPARENT, PRTFAGE,
            PESPOUSE, PEAFNOW, PRDTHSP, PUCHINHH, PULINENO, PRFAMNUM, PRFAMNUM,
            PRFAMREL, PRFAMTYP, PRMARSTA, PRPERTYP, PENATVTY, PEMNTVTY, PEFNTVTY,
            PRCITFLG, PRINUYER, PUSLFPRX, PEMLR, PUBUS2OT, PUBUSCK1, PUBUSCK2, 
            PUBUSCK3, PUBUSCK4, PURETOT, PERET1, PUDIS1, PUDIS2, PULAY, PEABSRSN,
            PEABSPDO, PEMJNUM, PEHRUSL1, PEHRUSL2, PEHRFTPT, PEHRUSLT, PEHRRSN1,
            PEHRRSN1, PEHRRSN2, PEHRRSN3, PUHROFF1, PUHROFF2, PUHROT1, PUHROT2,
            PEHRACT1, PEHRACT2, PEHRACTT, PEHRAVL, PEHRAVL, PUHRCK1, PUHRCK2,PUHRCK3,
            PUHRCK4, PUHRCK5, PUHRCK6, PUHRCK7, PUHRCK12, PULAYDT, PULAYDT, PULAY6M, PELAYAVL,
            PULAYAVR, PELAYLK, PELAYDUR, PELAYFTO, PULAYCK1, PULAYCK2, PULAYCK3, PULK, PULK,
            PELKM1, PULKM2, PULKM3, PULKM4, PULKM5, PULKM6, PULKDK1, PULKDK2, PULKPS1, PULKPS2,
            PELKAVL, PULKAVR, PELKLL1O, PELKLL2O, PELKLWO, PELKDUR, PELKFTO, PEDWWNTO, PEDWRSN,
            PEDWLKO, PEDWWK, PEDW4WK, PEDWLKWK, PEDWAVL, PEDWAVR, PUDWCK1, PUDWCK2, PUDWCK3, PUDWCK4,
            PUDWCK5, PEJHWKO, PUJHDP1O, PEJHRSN, PEJHWANT, PUJHCK1, PUJHCK2, PRABSREA, PRCIVLF, PRDISC,
            PREMPHRS, PREMPNOT, PREXPLF, PRFTLF, PRJOBSEA, PRPTHRS, PRPTREA, PRUNEDUR, PRUNTYPE, PRWKSCH,
            PRWKSTAT, PRWNTJOB, PUJHCK3, PUJHCK4, PUJHCK5, PUIODP1, PUIODP2, PUIODP3, PEIO1COW, PUIO1MFG,
            PEIO2COW, PUIO2MFG, PUIOCK1, PUIOCK2, PUIOCK3, PRIOELG, PRCOW1, PRCOW2, PRCOWPG, PRDTCOW1,
            PRDTCOW2, PRDTIND1, PRDTIND2, PRDTOCC1, PRDTOCC2, PREMP, PRMJIND1, PRMJIND2, PRMJOCGR, PRNAGPWS,
            PRNAGWS, PRSJMJ, PRERELG, PEERNUOT, PEERNPER, PEERNRT, PEERNHRY, PEERNH2, PEERNH1O, PRERNHLY, PTHR,
            PEERNHRO, PRERNWA, PTWK, PEERN, PUERN2, PTOT, PEERNWKP, PEERNCOV, PENLFJH, PENLFRET, PENLFACT,
            PUNLFCK1, PUNLFCK2, PESCHENR, PESCHFT, PESCHLVL, PRNLFSCH, PWFMWGT, PWLGWGT, PWORWGT, PWSSWGT,
            PWVETWGT, PRCHLD, PRNMCHLD, PRWERNAL, PRHERNAL, HXTENURE, HXHOUSUT, HXTELHHD, HXTELAVL, HXPHONEO,
            PXINUSYR, PXRRP, PXAGE, PXMARITL, PXSPOUSE, PXSEX, PXAFWHN1, PXAFNOW, PXEDUCA, PXRACE1, PXNATVTY,
            PXFNTVTY, PXHSPNON, PXMLR, PXRET1, PXABSRSN, PXABSPDO, PXMJOT, PXMJNUM, PXHRUSL1, PXHRUSL2, PXHRFTPT,
            PXHRUSLT, PXHRWANT, PXHRRSN1, PXHRRSN2, PXHRACT1, PXHRACT2, PXHRACTT, PXHRRSN3, PXHRAVL, PXLAYAVL,
            PXLAYLK, PXLAYDUR, PXLAYFTO, PXLKM1, PXLKAVL, PXLKLL1O, PXLKLL2O, PXLKLWO, PXLKDUR, PXLKFTO, PXDWWNTO,
            PXDWRSN, PXDWLKO, PXDWWK, PXDW4WK, PXDWLKWK, PXDWAVL, PXDWAVR, PXJHWKO, PXJHRSN, PXJHWANT, PXIO1COW,
            PXIO1ICD, PXIO1OCD, PXIO2COW, PXIO2ICD, PXIO2OCD, PXERNUOT, PXERNPER, PXERNH1O, PXERNHRO, PXERN, PXERNWKP,
            PXERNRT, PXERNHRY, PXERNH2, PXERNLAB, PXERNCOV, PXNLFJH, PXNLFRET, PXNLFACT, PXSCHENR, PXSCHFT, PXSCHLVL,
            QSTNUM, OCCURNUM, PEHGCOMP, PEGR6COR, PEMS123, PXDIPGED, PXHGCOMP, PXCYC, PXGRPROF, PXGR6COR,
            PXMS123, PWCMPWGT, PEIO1ICD, PEIO2ICD, PEIO2OCD, PRIMIND1, PRIMIND2, PEAFWHN1, PEAFWHN2,
            PEAFWHN3, PEAFWHN4, PXAFEVER, PELNDAD, PELNMOM, PEDADTYP, PEMOMTYP, PECOHAB, PXLNDAD,
            PXLNMOM, PXDADTYP, PXMOMTYP, PXCOHAB, PEDISEYE, PEDISREM, PEDISPHY, PEDISDRS, PEDISOUT, PRDISFLG,
            PXDISEAR, PXDISEYE, PXDISREM, PXDISPHY, PXDISDRS, PXDISOUT, HXFAMINC, PUNXTPR3, PRINTFLG, PWOWGT,
            PWTWGT, PWSWGT, PWAWGT, PWNWGT, VARSTRAT_PC, VARUNIT_PC, PXMNTVTY, PRMJOCC2, PEIO1OCD,
            PXPARENT,PXPARENT, PEIO1OCD)) # C = 387

# Dropping modules B and C ------------------------------------------------

processed_data <- processed_data %>% 
  select(-c(PTB1:PEB10, PEC11: PEC8A)) # C = 327

# Dropping core 1 questions -----------------------------------------------

processed_data <- processed_data %>% 
  select(-c(PEC1Q1A:PEC1Q14A1, PEC1Q14A4)) # C = 301

# Dropping predictors with too many NAs -----------------------------------

processed_data <- processed_data %>% 
  select(-c(PEC2Q1B, PEC2Q3B, PEC2Q4B, PEC2Q5C, PEC2Q5D, PEC2Q7B, PEC2Q8B, PEC2Q9B, PEC2Q10B, PEC2Q11B,
            PEC2Q12E, PTC2Q12F, PEC2Q12H, PEC2Q13B1, PEC2Q13B2, PEC2Q13B3, PEC2Q13B4, PEA1B, PEA2B, PEA3B,
            PEA4B, PED1B, PED2B, PED3B, PED4B, PED5B, PED6B, PED7B, PED9C, PED10B, PED12A, PED12B, PED13C,
            PED20B, PED21B, PED22B)) # until here all NAs, questions about spouse. C = 265

processed_data <- processed_data %>% 
  select(-c(PEE4D, OPERAN_PC, BALLETN_PC, PEE2D, PEC2Q12C, BOOKTYPE2_PC,PEE4B, PEE4C,
            PED13B, SINGING_OTHERS_PC, PEE8D, SALSAN_PC, PEE2B, PEE2C, PEE5D, PUBUS1, PEE1D,
            DANCEN_PC, PEE6D, PEE3D, PED9B, INSTRUMENT_OTHER_PC, PED8B, PEE8C, PEE8B, PEE7D, PEE6C, PEE6B, 
            PED8D, JAZZN_PC, PEE7B, PED18A, PEE7C, DOHYMN_PC, PED15A, DOCLASS_PC, PLAYN_PC,
            PEE5C, PEE5B, CLASSICALN_PC, PED17A, PED19A, DOSALSA_PC, DOSTAGE_PC, PED14A, DOJAZZ_PC,
            PED16A, DOOPERA_PC, PEE3C, PEE3B, PEE13, PEE15, PEE14, PUDIS, OWNART12_PC, MUSICALN_PC, 
            PEE1B, PEE1C, PEHRWANT, ARTMUSEUMN_PC, PED24B, PED23B, PED8C, PED24A, PED23A, PED8A,
            PUABSOT, PRHRUSL, PEGRPROF, PEC2Q2A10, POPROCK_PC, PTC2Q6A, PTC2Q12B, PEC2Q12D,
            BOOKSN_PC, NUMATTEND2_PC, BOOKSN2_PC, BOOKSTYPE2_PC, FICTION2_PC, NONFICTION_PC))
            # potentially interesting questions (a couple about work) most of which have more than 85% NA
            # because they were only asked to a subset of the sample based on a previous question. Also,
            # combined self/spouse questions. C = 185
       
# unsure whether to drop: PEC2Q2A1, PEC2Q2A2, PEC2Q2A3, PEC2Q2A4, PEC2Q2A5, PEC2Q2A6, PEC2Q2A7, PEC2Q2A8, PEC2Q2A9, PEC2Q2A11 (based on 3,827 cases)    
# same with combined partner questions about that: JAZZ2_PC, LATIN2_PC, CLASSICAL2_PC, OPERA2_PC, HYMNS2_PC, COUNTRY2_PC, RAP2_PC, 
# BLUES2_PC, FOLK2_PC, NONE2_PC

# Dropping factors because of sparsity ------------------------------------
# threshold at 400 observations

processed_data <- processed_data %>% 
  select(-c(GCTCB, PEMJOT, PRAGNA, PEDISEAR, PEDIPGED, PEERNLAB,
            PEC2Q2A2, PEC2Q2A1, PEC1Q14A3, PEC1Q14A2, PEC2Q2A11, PEC2Q2A9, PEC2Q2A8,
            PEC2Q2A7, PEC2Q2A6, PEC2Q2A5, PEC2Q2A4, PEC2Q2A3, PEC2Q8A, PEC2Q5B, PEC2Q5A, PEA2A,
            PEC2Q13A3, PEA8, PEA718, PED11A, PED10A, PED9A, PED7A, PED6A, PED5A, PEE2E, PEE2A,
            PED20A, PED13A, PEE8A, PEE7E, PEE6E, PEE5E, PEE4E, PEE4A, PEE3E, OPERA_PC, SALSA_PC,
            PEE8E, DANCE_PC, BALLET_PC, OPERA2_PC, CLASSICAL2_PC, LATIN2_PC, JAZZ2_PC, READPLAYS_PC,
            POETRY_PC, BALLET2_PC, NONE2_PC, FOLK2_PC, BLUES2_PC, RAP2_PC, COUNTRY2_PC, HYMNS2_PC,
            FILMFESTIVAL2_PC, DANCE2_PC, BOOKCLUB_PC, READPLAYS2_PC, ACTING_PC, INSTRUMENT_PC, 
            KNIT_PC, LMW_PC, POTTERY_PC, DONATE_PC, SUBSCRIBE_PC, TEACHART_PC, SINGING_PC, DANCING_PC,
            PECYC, PEE11B, PEE11A)) # C = 108

# Collapsing factors ------------------------------------------------------

processed_data <- processed_data %>% 
  mutate(HEFAMINC = fct_collapse(HEFAMINC,
                                 "< 30,000" = c("(01) Less than $5,000", "(02) 5,000 to 7,499", "(03) 7,500 to 9,999",
                                                "(04) 10,000 to 12,499", "(05) 12,500 to 14,999", "(06) 15,000 to 19,999",
                                                "(07) 20,000 to 24,999", "(08) 25,000 to 29,999"),
                                 "30,000-<60,000" = c("(09) 30,000 to 34,999", "(10) 35,000 to 39,999", "(11) 40,000 to 49,999",
                                                      "(12) 50,000 to 59,999"),
                                 "60,000-<100,000" = c("(13) 60,000 to 74,999", "(14) 75,000 to 99,999"),
                                 ">100,000" = c("(15) 100,000 to 149,999", "(16) 150,000 or more")),
         GTCBSASZ = fct_collapse(GTCBSASZ,
                                 "Not identified or nonmetropolitan" = "(0) Not identified or nonmetropolitan",
                                 "100,000-1,000,000" = c("(2) 100,000 - 249,999", "(3) 250,000 - 499,999", "(4) 500,000 - 999,999"),
                                 "1,000,000-4,999,999" = c("(5) 1,000,000 - 2,499,999", "(6) 2,500,000 - 4,999,999"),
                                 "5,000,000+" = "(7) 5,000,000+"),
         PEMARITL = fct_collapse(PEMARITL,
                                 "married" = c("(1) Married - spouse present", "(2) Married - spouse absent"),
                                 "previously married" = c("(3) Widowed", "(4) Divorced", "(5) Separated"),
                                 "never married" = "(6) Never married"),
         PEMARITL = fct_collapse(PEEDUCA,
                                "less than HS" = c("(31) Less than 1st grade", "(32) 1st, 2nd, 3rd or 4th grade",
                                                   "(33) 5th or 6th grade", "(34) 7th or 8th grade", "(35) 9th grade",
                                                   "(36) 10th grade", "(37) 11th grade", "(38) 12th grade no diploma"),
                                "HS or equivalent" = "(39) High school grad-diploma or equiv (GED)",
                                "some college or associate" = c("(40) Some college but no degree", 
                                                                "(41) Associate degree-occupational/vocational",
                                                                "(42) Associate degree-academic program"),
                                "Bachelor's or more" = c("(43) Bachelor's degree (ex: BA, AB, BS)", 
                                                         "(44) Master's degree (ex: MA, MS, MENG, MED, MSW)",
                                                         "(45) Professional school deg (ex: MD, DDS, DVM)",
                                                         "(46) Doctorate degree (ex: PHD, EDD)")),
         PTDTRACE = fct_collapse(PTDTRACE,
                                 "white" = "(01) White only",
                                 "black" = "(02) Black only",
                                 "other" = c("(03) American Indian, Alaskan Native only",
                                             "(04) Asian only", "(05) Hawaiian/Pacific Islander only",
                                             "(06) White-Black", "(07) White-AI", "(08) White-Asian", "(09) White-HP",
                                             "(10) Black-AI", "(11) Black-Asian", "(12) Black-HP", "(13) AI-Asian",
                                             "(14) AI-HP", "(15) Asian-HP", "(16) W-B-AI", "(17) W-B-A", "(18) W-B-HP",
                                             "(19) W-AI-A", "(20) W-AI-HP", "(21) W-A-HP", "(22) B-AI-A", "(23) W-B-AI-A",
                                             "(24) W-AI-A-HP", "(25) Other 3 race combinations", 
                                             "(26) Other 4 and 5 race combinations")),
         PRCITSHP = fct_collapse(PRCITSHP,
                                 "us by birth" = c("(1) Native, born in the United States",
                                                   "(2) Native, born in Puerto Rico or other U.S. island areas",
                                                   "(3) Native, born abroad of American parent or parents"),
                                 "foreign by birth" = c("(4) Foreign born, U.S. citizen by naturalization",
                                                        "(5) Foreign born, not a citizen of the United States")),
         PUWK = fct_collapse(PUWK,
                             "yes" = "(1) Yes",
                             "no" = c("(2) No", "(3) Retired", "(4) Disabled", "(5) Unable to work")),
         PRMJOCC1 = fct_collapse(PRMJOCC1,
                              "Management, business, and financial occupations" = "(01) Management, business, and financial occupations",
                              "Professional and related occupations" = "(02) Professional and related occupations",
                              "Service occupations" = "(03) Service occupations",
                              "Sales and related occupations" = "(04) Sales and related occupations",
                              "Office and administrative support occupations" = "(05) Office and administrative support occupations",
                              "Other--more'manual'" = c("(06) Farming, fishing, and forestry occupations",
                                                        "(07) Construction and extraction occupations",
                                                        "(08) Installation, maintenance, and repair occupations",
                                                        "(09) Production occupations",
                                                        "(10) Transportation and material moving occupations",
                                                        "(11) Armed forces"))) # debatable but there are 0 observations anyways

# Re-naming variables -----------------------------------------------------

processed_data <- processed_data %>% 
  rename(family_income = HEFAMINC,
         persons_living_household = HRNUMHOU,
         person_household_has_business_or_farm = HUBUS,
         region = GEREG,
         principal_city_balance_status = GTCBSAST,
         metropolitan_area_size = GTCBSASZ,
         age = PRTAGE,
         marital_status = PEMARITL,
         sex = PESEX,
         served_armed_forces = PEAFEVER,
         highest_education = PEEDUCA,
         race = PTDTRACE,
         hispanic = PEHSPNON,
         citizenship = PRCITSHP,
         worked_last_week = PUWK,
         occupation = PRMJOCC1,
         attend_live_music_performance = PEC2Q1A, # CORE 2 STARTS HERE
         attend_live_musical_2 = PEC2Q3A, # check v PEC1Q5A
         attend_live_non_musical_play = PEC2Q4A,
         go_art_exhibit = PEC2Q7A,
         go_arts_cultural_fair_festival = PEC2Q9A,
         go_see_buildings_neighborhoods = PEC2Q10A,
         visited_historic_park_monument_value = PEC2Q11A, # check v PEC1Q12A
         read_books_2 = PEC2Q12A, # check v PEC1Q13A
         read_novels_short_stories_2 = PEC2Q13A1, # check v PEC1Q14A1
         read_poetry = PEC2Q13A2,
         read_none_of_the_above_2 = PEC2Q13A4, # check v PEC1Q14A4 # CORE 2 ENDS HERE
         go_live_book_reading_poetry_storytelling = PEA1A, # MODULE A STARTS HERE
         go_movies_film = PEA3A,
         go_sports = PEA4A,
         attend_free_music_theater_dance = PEA5,
         go_art_exhibit_music_theater_dance_college_university_campus = PEA61,
         go_art_exhibit_music_theater_dance_school= PEA62,
         go_art_exhibit_music_theater_dance_religious_place = PEA63,
         go_art_exhibit_music_theater_dance_auditorium = PEA64,
         go_art_exhibit_music_theater_dance_restaurant_bar_club_coffee = PEA65,
         go_art_exhibit_music_theater_dance_museum_gallery = PEA66,
         go_art_exhibit_music_theater_dance_park_open_air = PEA67,
         go_art_exhibit_music_theater_dance_community_center = PEA68,
         go_art_exhibit_music_theater_dance_none_above = PEA69,
         like_listen_classical = PEA71,
         like_listen_opera = PEA72,
         like_listen_broadway_show_tunes = PEA73,
         like_listen_jazz = PEA74,
         like_listen_classic_rock_oldies = PEA75,
         like_listen_alternative_indie_rock = PEA76,
         like_listen_pop = PEA77,
         like_listen_country = PEA78,
         like_listen_edm = PEA79,
         like_listen_rap = PEA710,
         like_listen_reggae = PEA711,
         like_listen_blues_rnb_soul = PEA712,
         like_listen_latin_spanish_salsa = PEA713,
         like_listen_asian_african_middle_eastern = PEA714,
         like_listen_bluegrass = PEA715,
         like_listen_folk = PEA716,
         like_listen_hymns_gospel_choir = PEA717,
         like_listen_none_above = PEA719, # MODULE A ENDS HERE
         exercise_participate_sports = PED1A, # MODULE D STARTS HERE
         hunting_fishing = PED2A,
         participate_outdoor_activities = PED3A,
         indoor_plants_gardening = PED4A,
         social_dancing = PED11B,
         participate_community_activities = PED21A,
         volunteer_charity = PED22A, # MODULE D ENDS HERE
         lessons_classes_music = PEE1A, # MODULE E STARTS HERE
         learned_music_other_means = PEE1E,
         lessons_other_visual_arts = PEE3A,
         lessons_dance = PEE5A,
         lessons_creative_writing = PEE6A,
         lessons_art_art_history = PEE7A,
         child_go_art_museum_gallery = PEE9,
         child_live_music_theater_dance = PEE10,
         number_children_5_17 = PTE12, # MODULE E ENDS HERE
         attend_live_jazz_combined = JAZZ_PC,
         attend_live_classical_combined = CLASSICAL_PC,
         attend_live_musical_combined = MUSICAL_PC,
         attend_live_non_musical_play_combined = PLAY_PC,
         attend_other_music_dance_performance_combined = OTHERART_PC,
         visited_art_museum_combined = ARTMUSEUM_PC,
         visited_craft_fair_visual_arts_festival_combined = CRAFTFAIR_PC,
         visited_outdoor_fest_performing_artists_combined = OUTDOORFESTIVAL_PC,
         visited_historic_park_monument_combined = PARK_PC,
         read_books_combined = BOOKS_PC,
         read_novels_short_stories_combined = NOVELS_PC,
         read_none_of_the_above_combined = READNONE_PC,
         attend_live_music_performance_combined = LIVEMUSIC2_PC,
         attend_live_musical_combined2 = MUSICAL2_PC, # check v MUSICAL_PC
         attend_live_non_musical_play_combined_2 = PLAY2_PC, # check v PLAY_PC
         go_art_exhibit_combined = VISUALART2_PC,
         go_arts_cultural_fair_festival_combined = CULTURALFAIR2_PC,
         go_see_buildings_neighborhoods_combined = ARCHDESIGN2_PC,
         visited_historic_park_monument_value_combined = PARKS2_PC, # check v PARK_PC
         read_books_combined_2 = BOOKS2_PC, # check v BOOKS_PC
         read_novels_short_stories_combined_2 = NOVELS2_PC, # check v NOVELS_PC
         read_poetry_combined = POETRY2_PC,
         read_none_of_the_above_combined_2 = READHOME2_PC, # check v READNONE_PC
         go_live_book_reading_poetry_storytelling_combined = READINGEVENT_PC,
         go_movies_film_combined = MOVIES_PC,
         go_sports_combined = SPORTS_PC,
         exercise_participate_sports_combined = SPORTSACT_PC,
         hunting_fishing_combined = HUNT_PC,
         participate_outdoor_activities_combined = CAMP_PC,
         indoor_plants_gardening_combined = GARDEN_PC,
         own_art_combined = OWNART_PC,
         social_dancing_combined = SOCIALDANCE_PC,
         participate_community_activities_combined = COMMUNITY_PC,
         volunteer_charity_combined = VOLUNTEER_PC)

# Keeping only core 2 -----------------------------------------------------
# we explored keeping core 2 + mods A + D and core 2 + mods A + E, but the sample sizes were too small

core_2 <- processed_data %>% 
  select(-c(lessons_classes_music:number_children_5_17,exercise_participate_sports:volunteer_charity, 
            exercise_participate_sports_combined:volunteer_charity_combined)) # C = 84 
# 50% NA (so N~4,600) on attend_live_music_performance:read_none_of_the_above_2, attend_live_jazz_combined:read_none_of_the_above_combined_2 (core 2 questions)
# so 34/84 variables have 50% NA

# Imputing NAs ------------------------------------------------------------
# "NR" for factors and means for numerical predictors

for (i in seq_along(core_2)) {
  
  if (is.factor(core_2[[i]])) {
    
    core_2[[i]] <- factor(core_2[[i]], levels = c(levels(core_2[[i]]), "NR"))
    core_2[[i]][is.na(core_2[[i]])] <- "NR"
    
  } else {
    
    core_2[[i]][is.na(core_2[[i]])] <- mean(core_2[[i]], na.rm = TRUE)
    
  }
  
}

# Dropping "NR" in responses ----------------------------------------------

for (i in c(41:58)) {
  
  core_2[[i]] <- droplevels(core_2[[i]])
  
}

# Split into training and test sets ---------------------------------------
# high proportion for test given the high proportion of NAs on core 2 questions

core_2_train <- core_2 %>% sample_frac(0.6) # N 5,549

core_2_test <- core_2 %>% setdiff(core_2_train) # N 3,700

# Saving data -------------------------------------------------------------

saveRDS(core_2_train, file = "core_2_train.rds")

saveRDS(core_2_test, file = "core_2_test.rds")

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# END OF REPLICATION CODE -------------------------------------------------

# EXPLORATORY CODE--NOT NEEDED FOR REPLICATION ----------------------------

# Exploring missing data --------------------------------------------------

# processed_data ----------------------------------------------------------

missing <- tibble(
  var_name = character(),
  sum_na = numeric(),
  prop_missing = numeric()
)

for (i in seq_along(processed_data)) {
  
  missing[ i, 1] <- names(processed_data)[i]
  missing[ i, 2] <- sum(is.na(processed_data[ , i]))
  missing[i, 3] <- sum(is.na(processed_data[ , i])) / nrow(processed_data)
  
}

print(missing, n = Inf)

print(missing %>% arrange(desc(prop_missing)), n = Inf)

# mod_a_d -----------------------------------------------------------------

missing <- tibble(
  var_name = character(),
  sum_na = numeric(),
  prop_missing = numeric()
)

for (i in seq_along(mod_a_d)) {
  
  missing[ i, 1] <- names(mod_a_d)[i]
  missing[ i, 2] <- sum(is.na(mod_a_d[ , i]))
  missing[i, 3] <- sum(is.na(mod_a_d[ , i])) / nrow(mod_a_d)
  
}

print(missing, n = Inf)

print(missing %>% arrange(desc(prop_missing)), n = Inf)

#  mod_a_e ----------------------------------------------------------------

missing <- tibble(
  var_name = character(),
  sum_na = numeric(),
  prop_missing = numeric()
)

for (i in seq_along(mod_a_e)) {
  
  missing[ i, 1] <- names(mod_a_e)[i]
  missing[ i, 2] <- sum(is.na(mod_a_e[ , i]))
  missing[i, 3] <- sum(is.na(mod_a_e[ , i])) / nrow(mod_a_e)
  
}

print(missing, n = Inf)

print(missing %>% arrange(desc(prop_missing)), n = Inf)

# core_2 ------------------------------------------------------------------

missing <- tibble(
  var_name = character(),
  sum_na = numeric(),
  prop_missing = numeric()
)

for (i in seq_along(core_2)) {
  
  missing[ i, 1] <- names(core_2)[i]
  missing[ i, 2] <- sum(is.na(core_2[ , i]))
  missing[i, 3] <- sum(is.na(core_2[ , i])) / nrow(core_2)
  
}

print(missing, n = Inf)

print(missing %>% arrange(desc(prop_missing)), n = Inf)

# Checking for possible sparsity across categories ------------------------

for (i in 1:10) {
  
  print(names(processed_data[i]))
  print(table(processed_data[i]))
  
}

# Checking data types -----------------------------------------------------

types <- tibble(
  var_name = character(),
  type = character(),
)

for (i in seq_along(core_2)) {
  
  types[ i, 1] <- names(core_2)[i]
  types[ i, 2] <- typeof(core_2[[i]])
  
}

print(types %>% arrange(type), n = Inf)

# 4 numerical: HRNUMHOU, PRTAGE, PTC1Q13B, PTE12

# Looking at factors to collapse ------------------------------------------

factors_to_collapse <- c("HEFAMINC", "GTCBSASZ", "PEMARITL", "PEEDUCA", "PTDTRACE", 
                         "PRCITSHP", "PUWK", "PRMJOCC1")

for (i in 1:length(factors_to_collapse)) {
  
  print(table(processed_data[[factors_to_collapse[[i]]]]))
  
}

# Visualizing responses ---------------------------------------------------

for (i in seq_along(processed_data[ , which( colnames(processed_data)=="like_listen_classical" ):which( colnames(processed_data)=="like_listen_none_above" )])) { #PEA71 and PEA719
  
  plot <- ggplot(processed_data[ , which( colnames(processed_data)=="like_listen_classical" ):which( colnames(processed_data)=="like_listen_none_above" )]) +
    aes(processed_data[ , which( colnames(processed_data)=="like_listen_classical" ):which( colnames(processed_data)=="like_listen_none_above" )][[i]]) +
    geom_bar() +
    theme_bw() +
    theme(panel.border = element_blank()) +
    xlab(paste("Distribution of", colnames(processed_data[ , which( colnames(processed_data)=="like_listen_classical" ):which( colnames(processed_data)=="like_listen_none_above" )])[i]))
  
  ggsave(plot, path = "distribution_responses", 
         filename = paste(colnames(processed_data[ , which( colnames(processed_data)=="like_listen_classical" ):which( colnames(processed_data)=="like_listen_none_above" )])[i]), device = "png")
  
}

# Dividing datasets -------------------------------------------------------
# mod d k = 15, mod e k = 9 
# we explored keeping core 2 + mods A + D and core 2 + mods A + E, but the sample sizes were too small

mod_a_d <- processed_data %>% 
  select(-c(lessons_classes_music:number_children_5_17)) # C = 99 

mod_a_e <- processed_data %>% 
  select(-c(exercise_participate_sports:volunteer_charity, exercise_participate_sports_combined:volunteer_charity_combined)) # C = 93

# Dropping NAs on mods D and E variables ----------------------------------

mod_a_d <- mod_a_d[complete.cases((mod_a_d[ , which( colnames(mod_a_d)=="exercise_participate_sports" ):which( colnames(mod_a_d)=="volunteer_charity" )])),]
# N = 2,235. max N combining mod A and D = 2,382, so no mod D variable that we kept has more than 6.2% NAs
# 50% NA (so N~1,100) attend_live_music_performance:read_none_of_the_above_2, attend_live_jazz_combined:read_none_of_the_above_combined_2 (core 2 questions)

mod_a_e <- mod_a_e[complete.cases((mod_a_e[ , which( colnames(mod_a_e)=="lessons_classes_music" ):which( colnames(mod_a_e)=="number_children_5_17" )])),]
# N = 2,177. max N combining mod A and D = 2,456, so no mod E variable that we kept has more than 11.3% NAs
# 50% NA on the same as mod_a_d (so N~1,000)
