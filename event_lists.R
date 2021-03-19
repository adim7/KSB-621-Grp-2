# Creating lists of potential event categories from the Event Desc and Event Title Variables

`Religious/Congregation` <- c("Church|Service|Worship|Religious|Christian|Bible|Ministry|Ministries|Chorus|Jehovah
                              |Gospel|Tabernacle|Prayer|Easter|Christ|40 days|Christmas|Good News Club|Sunday School
                              |Baptist|Archdioc|Retreat|God|Gods|St Marys|Chapel|Islamic|Temple|Fellowship|Choir|Sikh
                              |Sanskriti|Scripture|WIZARD|Hosanna,Baptism|Fellowship|Choir|Prayers|Devotional")

`Vocational/Mentoring` <- c("After-School|Afterschool|After|Mentoring|Nehemiah|Summer Program|Machine Sewing|Chinese School
                            |Language|Kids Club|Adult Ed|Camp for Girls|SBC Summer 19|Saturday Classes|eMode Summer Camp
                            |CODE Summer Camp|Cooking Enrichment|Guitar|Driver Improvement|Brain Builders|LEGO|Video Game Design
                            |French|Boys & Girls Club|Drivers Ed|Boy Scouts|CPR|Sewing|Nursery|Little Coders|Apprenticeship
                            |Lifeguard|Language|Motorcycle|Summertime|Brothers Keeper|KVenture|John Lucas|Blood Drive|Kid Millionaire
                            |Early Childhood|Maximum Potential")


`Social/Cultural Events` <- c("Meeting|Gathering|Board|Public|Council|Annual|Community|General|Organization|Event|Ceremony
                             |Austin Tamizh Sangam|Kannada Sangha Deepavali|Forum|Carnival|PAC|Informational Session|Anniversary
                             |Members|Association|Concert|Client|Banquet|Festival|Meets Every|Town Hall|HOA|Fundraiser|Girls Scout
                             |Boys Scout|Dinner|Cultural|Duplicate Bridge|Market|Open House|Informational Session|Wedding Reception
                             |Relay for Life|Sorority|Delta Sigma Theta|Gamma Phi Beta|Kwanzaa|Fair|Wedding|Potluck|Reunion|Campaign
                             |Rotary|PTA|Gala|Parade|Giveaway|Thanksgiving|Family|Families|Election|Celebration|Black History|Bingo
                             |Cultural Event|Wedding Reception|Monthly|Meeting|Meetings|Mtg|Party|Gala|Businesses|Fair|Reception
                             |Lunar New Year|City Hall|Health & Wellness|PARENTS|Open House|Fundraiser|Recreation|Clinic|Rotary 
                             club|Filipino|PTA|PTSA|Springfest|Annual event|YMCA|SnowFest|BingoNight|AutumnFest|Mayors Breakfast|Anniversary
                             |Gun Safety|Seeds of Change|Ceremony")

`Academic/Professional` <- c("Elementary|High|Middle|Classroom|Exam|Laboratory|Lab|Robotics|Education|Academy|Lecture|Develop
                             ment|Course|Graduation|Seminar|ENG|PLT|SPA|SPA|COM|Scholars|Professional|Learning|Workshop|Scholarship
                             |Research|Job Fair|Advocacy|Summit|Luncheon|Conference|College Classes|University Classes|SAT Bootcamp
                             |Advisory|Collin College|Coding|Tamil School|Symposium|School Leadership|Blockchain|Testing|Campus
                             |Developer Boot Camp|Literacy|Apprenticeship|Science|Consulting|STEM|State|District|Tribune|SAT|Homework
                             |Game Development|GED|Financial|Medical|Welding & Metal|Photography|Math|Leadercast|Presentation
                             |Spelling Bee|Study|Kindergardten|luncheon|Mentor|Conference|Presentation|Panel|Workforce|Working Group
                             |Workshops|Professional Development|Creativity|Science|Grade|Convention|STEM|Truman College|Interview
                             |keynote|Talk|TED|Session|Exam|Implicit Bias|Research|Department|Coalition|National|Framework|Financial
                             |College readiness|Academic|Medical|Welding & Metal|Photography|Animation|Introduction|Speech|Seminar|Orientation
                             |Texas Tech|Government|Psychology|Extreme Fire Solutions|Small Business|Management Institute|Dentist|Scholar Program
                             |Business Institute|SIU|Business|Rush University|Police|Kaplan Test Prep|Southern Illinois University|Taekwondo
                             |Diamondback|Fire Dept")
       
`Sports/Fitness` <- c("Basketball|Athletic|League|Practice|Running|Run|Games|Soccer|Baseball|Lacrosse|Swim|Scout|Karate|Martial|Arts
                      |Chess|Olympics|Field|Gym|Fitness|Bicycle|Track|Volleyball|Football|Hockey|Sports|Tennis|Marching|Work Outs
                      |Workout|Puma|Team|Wrestling|Polo|Court|Tournament|Tourney|Galacticos|Pole Vault|Aquatic|Rugby|Batting|Futsal|Diving
                      |Softball|Triathletes|Championships|Champ|Gulf|NFL|Game Day|Training|Derby|Practicd|Gymnastics|Scrimmage|Walk
                      |Flagler Fury|Club Vaultitude|Soccer Club|Baseball Club|Club Practice|Black Belt|BLACKBELT|Coaches|Superbowl
                      |Hoops|Hoop|Bike Ride|Tryouts|VBall|Extreme 212|Dribblers|ATX Future|BBall|San Antonio Spurs|Spartans|Playoff
                      |Cross Country|Fball|Complete Elite|Yoga|Cross Fit|Texans|Fencing|GTX Bobcats|Bowl|Phoenix Rising|Tru Vizion
                      |NTX Tarheels Elite|Bike Camps|Cheer|woodlands marlins|Elite|Mini Camp|Elite Dawg|Try Outs|B Ball|Swing|Tourney
                      |WHST|WH1|WR|WR1|HC|VV|WRT|WH3|WHBB|WH|HC|FT|WHBC|BP|HCT|WH2|WYBA|All Star|Battle|Jaguars|Turf|Roughnecks|HJBB|BB 
                      game|Multisport|Golf|Pickleball|KungFu|JUSA|Meet|Intrasquad|World Series|Outdoor|Zumba|Capoeira|Agility|Weight|OCWPF
                      |VS|Legends|SC Revolution|Babe Ruth|Judo|Qualifier|Game|Unicycle|Sport|Grand Prix|Tournament|Basketball|Football
                      |Swim|Track|Practice|Soccer|Try Outs|Volleyball|Olympics|SC Revolution|Legends FC|Scrimmage|Softball|Tennis|Martial
                      |Fitness|Conditioning|Coaches|Rockwall Warriors|Club Vertical|Prolyfe|Boxing|Dribbler|CampGlad|Playoffs|BasketB|VolleyB
                      |MLK Dream Classic|Cricket|Devils|Grizzlies|Tae Kwon Do")                  

`Performing/Arts` <- c("Dance|Music|Art|Craft|Theater|Theatre|Recitals|Recital|Performances|LegallyBlonde|Aladdin|Ensembles|Film|Play
                      |Recording Session|Stage|Rehearsal|Act|Tributes|Band|Jazz|Show|Rehersal|Percussion|Culinary|Orchestra|Audition
                      |Movie|Drama|Ballet|Symphony|Circuit Contest|StarQuest|Violet Crown Independent|Philharmonic|Talent|Entertainment
                      |Unleash The Beast|Debate Club|Debate|WSCO|Cabaret|Production|Piano|Pianos|Opera|Ukulele|Rembrandts|Fashion
                      |Saxophone|Hip Hop|Drum|Miss|Pageant|Rhythm|Cinderella|KidVenture|Summerstock|Summer Stock|Origami|Excelsior Awaken
                      |Arts Camp|Rehearsal|Concert|Dance Team|Stage|Radio|Studio|Theater|Sitcom|Oratorical|Hip Hop|TALENT|Cheerleading
                      |Orchestra|Performance|Dance|Prince & Princess|Illusion|Dancing")



