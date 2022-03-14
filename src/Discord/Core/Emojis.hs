module Discord.Core.Emojis where
import Discord.API.Internal.Types.Guild (Emoji(..))
import Data.Text (Text)


standardEmoji :: Text -> Emoji
standardEmoji name = Emoji { emojiId         = Nothing 
                           , emojiName       = Just name
                           , emojiCreator    = Nothing
                           , emojiIsAnimated = False
                           , emojiIsManaged  = False
                           , emojiRoleIds    = [] 
                           }


grinning :: Emoji
grinning = standardEmoji "\128512" 

smiley :: Emoji
smiley = standardEmoji "\128515" 

smile :: Emoji
smile = standardEmoji "\128516" 

grin :: Emoji
grin = standardEmoji "\128513" 

laughing :: Emoji
laughing = standardEmoji "\128518" 

sweatSmile :: Emoji
sweatSmile = standardEmoji "\128517" 

joy :: Emoji
joy = standardEmoji "\128514" 

rofl :: Emoji
rofl = standardEmoji "\129315" 

relaxed :: Emoji
relaxed = standardEmoji "\9786\65039"

blush :: Emoji
blush = standardEmoji "\128522" 

innocent :: Emoji
innocent = standardEmoji "\128519" 

slightSmile :: Emoji
slightSmile = standardEmoji "\128578"

upsideDown :: Emoji
upsideDown = standardEmoji "\128579"

disappointedRelieved :: Emoji
disappointedRelieved = standardEmoji "\128549"

wink :: Emoji
wink = standardEmoji "\128521"

sweat :: Emoji
sweat = standardEmoji "\128531"

hugging :: Emoji
hugging = standardEmoji "\129303"

thinking :: Emoji
thinking = standardEmoji "\129300"

relieved :: Emoji
relieved = standardEmoji "\128524"

pensive :: Emoji
pensive = standardEmoji "\128532"

faceWithHandOverMouth :: Emoji
faceWithHandOverMouth = standardEmoji "\129325"

worried :: Emoji
worried = standardEmoji "\128543"

smilingFaceWithTear :: Emoji
smilingFaceWithTear = standardEmoji "\129394"

yawningFace :: Emoji
yawningFace = standardEmoji "\129393"

confused :: Emoji
confused = standardEmoji "\128533"

shushingFace :: Emoji
shushingFace = standardEmoji "\129323"

heartEyes :: Emoji
heartEyes = standardEmoji "\128525"

slightFrown :: Emoji
slightFrown = standardEmoji "\128577"

smilingFaceWith3Hearts :: Emoji
smilingFaceWith3Hearts = standardEmoji "\129392"

lyingFace :: Emoji
lyingFace = standardEmoji "\129317"

frowning2 :: Emoji
frowning2 = standardEmoji "\9785\65039"

noMouth :: Emoji
noMouth = standardEmoji "\128566"

kissingHeart :: Emoji
kissingHeart = standardEmoji "\128536"

expressionless :: Emoji
expressionless = standardEmoji "\128529"

persevere :: Emoji
persevere = standardEmoji "\128547"

kissing :: Emoji
kissing = standardEmoji "\128535"

confounded :: Emoji
confounded = standardEmoji "\128534"

tiredFace :: Emoji
tiredFace = standardEmoji "\128555"

kissingSmilingEyes :: Emoji
kissingSmilingEyes = standardEmoji "\128553"

weary :: Emoji
weary = standardEmoji "\128537"

pleadingFace :: Emoji
pleadingFace = standardEmoji "\129402"

kissingClosedEyes :: Emoji
kissingClosedEyes = standardEmoji "\128538"

cry :: Emoji
cry = standardEmoji "\128546"

yum :: Emoji
yum = standardEmoji "\128523"

sob :: Emoji
sob = standardEmoji "\128557"

triumph :: Emoji
triumph = standardEmoji "\128548"

stuckOutTongue :: Emoji
stuckOutTongue = standardEmoji "\128539"

faceExhaling :: Emoji
faceExhaling = standardEmoji "\128558\8205\128168"

angry :: Emoji
angry = standardEmoji "\128544"

stuckOutTongueClosedEyes :: Emoji
stuckOutTongueClosedEyes = standardEmoji "\128541"

rage :: Emoji
rage = standardEmoji "\128545"

stuckOutTongueWinkingEye :: Emoji
stuckOutTongueWinkingEye = standardEmoji "\128540"

faceWithSymbolsOverMouth :: Emoji
faceWithSymbolsOverMouth = standardEmoji "\129324"

explodingHead :: Emoji
explodingHead = standardEmoji "\129327"

zanyFace :: Emoji
zanyFace = standardEmoji "\129322"

grimacing :: Emoji
grimacing = standardEmoji "\128556"

thermometerFace :: Emoji
thermometerFace = standardEmoji "\129298"

headBandage :: Emoji
headBandage = standardEmoji "\129301"

rollingEyes :: Emoji
rollingEyes = standardEmoji "\128580"

moneyMouth :: Emoji
moneyMouth = standardEmoji "\129297"

hushed :: Emoji
hushed = standardEmoji "\128559"

cowboy :: Emoji
cowboy = standardEmoji "\129312"

frowning :: Emoji
frowning = standardEmoji "\128550"

disguisedFace :: Emoji
disguisedFace = standardEmoji "\129400"

anguished :: Emoji
anguished = standardEmoji "\128551"

smilingImp :: Emoji
smilingImp = standardEmoji "\128520"

imp :: Emoji
imp = standardEmoji "\128127"

openMouth :: Emoji
openMouth = standardEmoji "\128558"

japaneseOgre :: Emoji
japaneseOgre = standardEmoji "\128121"

japaneseGoblin :: Emoji
japaneseGoblin = standardEmoji "\128122"

astonished :: Emoji
astonished = standardEmoji "\128562"

sleeping :: Emoji
sleeping = standardEmoji "\128564"

droolingFace :: Emoji
droolingFace = standardEmoji "\129316"

flushed :: Emoji
flushed = standardEmoji "\128563"

faceInClouds :: Emoji
faceInClouds = standardEmoji "\128566\8205\127787\65039"

hotFace :: Emoji
hotFace = standardEmoji "\129397"

clown :: Emoji
clown = standardEmoji "\129313"

coldFace :: Emoji
coldFace = standardEmoji "\129398"

scream :: Emoji
scream = standardEmoji "\128561"

poop :: Emoji
poop = standardEmoji "\128169"

fearful :: Emoji
fearful = standardEmoji "\128552"

ghost :: Emoji
ghost = standardEmoji "\128123"

coldSweat :: Emoji
coldSweat = standardEmoji "\128560"

skull :: Emoji
skull = standardEmoji "\128128"

skullCrossbones :: Emoji
skullCrossbones = standardEmoji "\9760\65039"

nauseatedFace :: Emoji
nauseatedFace = standardEmoji "\129314"

faceVomiting :: Emoji
faceVomiting = standardEmoji "\129326"

zipperMouth :: Emoji
zipperMouth = standardEmoji "\129296"

palmsUpTogether :: Emoji
palmsUpTogether = standardEmoji "\129330"

openHands :: Emoji
openHands = standardEmoji "\128080"

raisedHands :: Emoji
raisedHands = standardEmoji "\128588"

clap :: Emoji
clap = standardEmoji "\128079"

handshake :: Emoji
handshake = standardEmoji "\129309"

thumbsUp :: Emoji
thumbsUp = standardEmoji "\128077"

thumbsDown :: Emoji
thumbsDown = standardEmoji "\128078"

punch :: Emoji
punch = standardEmoji "\128074"

fist :: Emoji
fist = standardEmoji "\9994"

leftFacingFist :: Emoji
leftFacingFist = standardEmoji "\129307"

rightFacingFist :: Emoji
rightFacingFist = standardEmoji "\129308"

fingersCrossed :: Emoji
fingersCrossed = standardEmoji "\129310"

vHand :: Emoji
vHand = standardEmoji "\9996\65039"

loveYouGesture :: Emoji
loveYouGesture = standardEmoji "\129311"

metal :: Emoji
metal = standardEmoji "\129304"

okHand :: Emoji
okHand = standardEmoji "\128076"

pinchingHand :: Emoji
pinchingHand = standardEmoji "\129295"

pinchedFingers :: Emoji
pinchedFingers = standardEmoji "\129292"

pointLeft :: Emoji
pointLeft = standardEmoji "\128072"

pointRight :: Emoji
pointRight = standardEmoji "\128073"

pointUp2 :: Emoji
pointUp2 = standardEmoji "\128070"

pointDown :: Emoji
pointDown = standardEmoji "\128071"

pointUp :: Emoji
pointUp = standardEmoji "\9757\65039"

raisedHand :: Emoji
raisedHand = standardEmoji "\9995"

raisedBackOfHand :: Emoji
raisedBackOfHand = standardEmoji "\129306"

handSplayed :: Emoji
handSplayed = standardEmoji "\128400\65039"

vulcanHand :: Emoji
vulcanHand = standardEmoji "\128406"

wave :: Emoji
wave = standardEmoji "\128075"

callMe :: Emoji
callMe = standardEmoji "\129305"

muscle :: Emoji
muscle = standardEmoji "\128170"

middleFinger :: Emoji
middleFinger = standardEmoji "\128405"

pray :: Emoji
pray = standardEmoji "\128591"

eyes :: Emoji
eyes = standardEmoji "\128064"

faceWithRaisedEyebrow :: Emoji
faceWithRaisedEyebrow = standardEmoji "\129320"

faceWithMonocle :: Emoji
faceWithMonocle = standardEmoji "\129488"

nerd :: Emoji
nerd = standardEmoji "\129299"

sunglasses :: Emoji
sunglasses = standardEmoji "\128526"

starStruck :: Emoji
starStruck = standardEmoji "\129321"

partyFace :: Emoji
partyFace = standardEmoji "\129395"

smirk :: Emoji
smirk = standardEmoji "\128527"

unamused :: Emoji
unamused = standardEmoji "\128530"

disappointed :: Emoji
disappointed = standardEmoji "\128542"