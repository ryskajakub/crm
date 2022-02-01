{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Glyphicon where

import "fay-base" Data.Text (Text, fromString, (<>))

import HaskellReact

glyphicon :: Text -> DOMElement
glyphicon glyphIdentificator = span'
  (class'' ["glyphicon", "glyphicon-" <> glyphIdentificator]) 
  ([]::[DOMElement])


asterisk :: DOMElement
asterisk = glyphicon "asterisk"

plus :: DOMElement
plus = glyphicon "plus"

euro :: DOMElement
euro = glyphicon "euro"

minus :: DOMElement
minus = glyphicon "minus"

cloud :: DOMElement
cloud = glyphicon "cloud"

envelope :: DOMElement
envelope = glyphicon "envelope"

pencil :: DOMElement
pencil = glyphicon "pencil"

glass :: DOMElement
glass = glyphicon "glass"

music :: DOMElement
music = glyphicon "music"

search :: DOMElement
search = glyphicon "search"

heart :: DOMElement
heart = glyphicon "heart"

star :: DOMElement
star = glyphicon "star"

starEmpty :: DOMElement
starEmpty = glyphicon "star-empty"

user :: DOMElement
user = glyphicon "user"

film :: DOMElement
film = glyphicon "film"

thLarge :: DOMElement
thLarge = glyphicon "th-large"

th :: DOMElement
th = glyphicon "th"

thList :: DOMElement
thList = glyphicon "th-list"

ok :: DOMElement
ok = glyphicon "ok"

remove :: DOMElement
remove = glyphicon "remove"

zoomIn :: DOMElement
zoomIn = glyphicon "zoom-in"

zoomOut :: DOMElement
zoomOut = glyphicon "zoom-out"

off :: DOMElement
off = glyphicon "off"

signal :: DOMElement
signal = glyphicon "signal"

cog :: DOMElement
cog = glyphicon "cog"

trash :: DOMElement
trash = glyphicon "trash"

home :: DOMElement
home = glyphicon "home"

file :: DOMElement
file = glyphicon "file"

time :: DOMElement
time = glyphicon "time"

road :: DOMElement
road = glyphicon "road"

downloadAlt :: DOMElement
downloadAlt = glyphicon "download-alt"

download :: DOMElement
download = glyphicon "download"

upload :: DOMElement
upload = glyphicon "upload"

inbox :: DOMElement
inbox = glyphicon "inbox"

playCircle :: DOMElement
playCircle = glyphicon "play-circle"

repeat :: DOMElement
repeat = glyphicon "repeat"

refresh :: DOMElement
refresh = glyphicon "refresh"

listAlt :: DOMElement
listAlt = glyphicon "list-alt"

lock :: DOMElement
lock = glyphicon "lock"

flag :: DOMElement
flag = glyphicon "flag"

headphones :: DOMElement
headphones = glyphicon "headphones"

volumeOff :: DOMElement
volumeOff = glyphicon "volume-off"

volumeDown :: DOMElement
volumeDown = glyphicon "volume-down"

volumeUp :: DOMElement
volumeUp = glyphicon "volume-up"

qrcode :: DOMElement
qrcode = glyphicon "qrcode"

barcode :: DOMElement
barcode = glyphicon "barcode"

tag :: DOMElement
tag = glyphicon "tag"

tags :: DOMElement
tags = glyphicon "tags"

book :: DOMElement
book = glyphicon "book"

bookmark :: DOMElement
bookmark = glyphicon "bookmark"

print :: DOMElement
print = glyphicon "print"

camera :: DOMElement
camera = glyphicon "camera"

font :: DOMElement
font = glyphicon "font"

bold :: DOMElement
bold = glyphicon "bold"

italic :: DOMElement
italic = glyphicon "italic"

textHeight :: DOMElement
textHeight = glyphicon "text-height"

textWidth :: DOMElement
textWidth = glyphicon "text-width"

alignLeft :: DOMElement
alignLeft = glyphicon "align-left"

alignCenter :: DOMElement
alignCenter = glyphicon "align-center"

alignRight :: DOMElement
alignRight = glyphicon "align-right"

alignJustify :: DOMElement
alignJustify = glyphicon "align-justify"

list :: DOMElement
list = glyphicon "list"

indentLeft :: DOMElement
indentLeft = glyphicon "indent-left"

indentRight :: DOMElement
indentRight = glyphicon "indent-right"

facetimeVideo :: DOMElement
facetimeVideo = glyphicon "facetime-video"

picture :: DOMElement
picture = glyphicon "picture"

mapMarker :: DOMElement
mapMarker = glyphicon "map-marker"

adjust :: DOMElement
adjust = glyphicon "adjust"

tint :: DOMElement
tint = glyphicon "tint"

edit :: DOMElement
edit = glyphicon "edit"

share :: DOMElement
share = glyphicon "share"

check :: DOMElement
check = glyphicon "check"

move :: DOMElement
move = glyphicon "move"

stepBackward :: DOMElement
stepBackward = glyphicon "step-backward"

fastBackward :: DOMElement
fastBackward = glyphicon "fast-backward"

backward :: DOMElement
backward = glyphicon "backward"

play :: DOMElement
play = glyphicon "play"

pause :: DOMElement
pause = glyphicon "pause"

stop :: DOMElement
stop = glyphicon "stop"

forward :: DOMElement
forward = glyphicon "forward"

fastForward :: DOMElement
fastForward = glyphicon "fast-forward"

stepForward :: DOMElement
stepForward = glyphicon "step-forward"

eject :: DOMElement
eject = glyphicon "eject"

chevronLeft :: DOMElement
chevronLeft = glyphicon "chevron-left"

chevronRight :: DOMElement
chevronRight = glyphicon "chevron-right"

plusSign :: DOMElement
plusSign = glyphicon "plus-sign"

minusSign :: DOMElement
minusSign = glyphicon "minus-sign"

removeSign :: DOMElement
removeSign = glyphicon "remove-sign"

okSign :: DOMElement
okSign = glyphicon "ok-sign"

questionSign :: DOMElement
questionSign = glyphicon "question-sign"

infoSign :: DOMElement
infoSign = glyphicon "info-sign"

screenshot :: DOMElement
screenshot = glyphicon "screenshot"

removeCircle :: DOMElement
removeCircle = glyphicon "remove-circle"

okCircle :: DOMElement
okCircle = glyphicon "ok-circle"

banCircle :: DOMElement
banCircle = glyphicon "ban-circle"

arrowLeft :: DOMElement
arrowLeft = glyphicon "arrow-left"

arrowRight :: DOMElement
arrowRight = glyphicon "arrow-right"

arrowUp :: DOMElement
arrowUp = glyphicon "arrow-up"

arrowDown :: DOMElement
arrowDown = glyphicon "arrow-down"

shareAlt :: DOMElement
shareAlt = glyphicon "share-alt"

resizeFull :: DOMElement
resizeFull = glyphicon "resize-full"

resizeSmall :: DOMElement
resizeSmall = glyphicon "resize-small"

exclamationSign :: DOMElement
exclamationSign = glyphicon "exclamation-sign"

gift :: DOMElement
gift = glyphicon "gift"

leaf :: DOMElement
leaf = glyphicon "leaf"

fire :: DOMElement
fire = glyphicon "fire"

eyeOpen :: DOMElement
eyeOpen = glyphicon "eye-open"

eyeClose :: DOMElement
eyeClose = glyphicon "eye-close"

warningSign :: DOMElement
warningSign = glyphicon "warning-sign"

plane :: DOMElement
plane = glyphicon "plane"

calendar :: DOMElement
calendar = glyphicon "calendar"

random :: DOMElement
random = glyphicon "random"

comment :: DOMElement
comment = glyphicon "comment"

magnet :: DOMElement
magnet = glyphicon "magnet"

chevronUp :: DOMElement
chevronUp = glyphicon "chevron-up"

chevronDown :: DOMElement
chevronDown = glyphicon "chevron-down"

retweet :: DOMElement
retweet = glyphicon "retweet"

shoppingCart :: DOMElement
shoppingCart = glyphicon "shopping-cart"

folderClose :: DOMElement
folderClose = glyphicon "folder-close"

folderOpen :: DOMElement
folderOpen = glyphicon "folder-open"

resizeVertical :: DOMElement
resizeVertical = glyphicon "resize-vertical"

resizeHorizontal :: DOMElement
resizeHorizontal = glyphicon "resize-horizontal"

hdd :: DOMElement
hdd = glyphicon "hdd"

bullhorn :: DOMElement
bullhorn = glyphicon "bullhorn"

bell :: DOMElement
bell = glyphicon "bell"

certificate :: DOMElement
certificate = glyphicon "certificate"

thumbsUp :: DOMElement
thumbsUp = glyphicon "thumbs-up"

thumbsDown :: DOMElement
thumbsDown = glyphicon "thumbs-down"

handRight :: DOMElement
handRight = glyphicon "hand-right"

handLeft :: DOMElement
handLeft = glyphicon "hand-left"

handUp :: DOMElement
handUp = glyphicon "hand-up"

handDown :: DOMElement
handDown = glyphicon "hand-down"

circleArrowRight :: DOMElement
circleArrowRight = glyphicon "circle-arrow-right"

circleArrowLeft :: DOMElement
circleArrowLeft = glyphicon "circle-arrow-left"

circleArrowUp :: DOMElement
circleArrowUp = glyphicon "circle-arrow-up"

circleArrowDown :: DOMElement
circleArrowDown = glyphicon "circle-arrow-down"

globe :: DOMElement
globe = glyphicon "globe"

wrench :: DOMElement
wrench = glyphicon "wrench"

tasks :: DOMElement
tasks = glyphicon "tasks"

filter :: DOMElement
filter = glyphicon "filter"

briefcase :: DOMElement
briefcase = glyphicon "briefcase"

fullscreen :: DOMElement
fullscreen = glyphicon "fullscreen"

dashboard :: DOMElement
dashboard = glyphicon "dashboard"

paperclip :: DOMElement
paperclip = glyphicon "paperclip"

heartEmpty :: DOMElement
heartEmpty = glyphicon "heart-empty"

link :: DOMElement
link = glyphicon "link"

phone :: DOMElement
phone = glyphicon "phone"

pushpin :: DOMElement
pushpin = glyphicon "pushpin"

usd :: DOMElement
usd = glyphicon "usd"

gbp :: DOMElement
gbp = glyphicon "gbp"

sort :: DOMElement
sort = glyphicon "sort"

sortByAlphabet :: DOMElement
sortByAlphabet = glyphicon "sort-by-alphabet"

sortByAlphabetAlt :: DOMElement
sortByAlphabetAlt = glyphicon "sort-by-alphabet-alt"

sortByOrder :: DOMElement
sortByOrder = glyphicon "sort-by-order"

sortByOrderAlt :: DOMElement
sortByOrderAlt = glyphicon "sort-by-order-alt"

sortByAttributes :: DOMElement
sortByAttributes = glyphicon "sort-by-attributes"

sortByAttributesAlt :: DOMElement
sortByAttributesAlt = glyphicon "sort-by-attributes-alt"

unchecked :: DOMElement
unchecked = glyphicon "unchecked"

expand :: DOMElement
expand = glyphicon "expand"

collapseDown :: DOMElement
collapseDown = glyphicon "collapse-down"

collapseUp :: DOMElement
collapseUp = glyphicon "collapse-up"

logIn :: DOMElement
logIn = glyphicon "log-in"

flash :: DOMElement
flash = glyphicon "flash"

logOut :: DOMElement
logOut = glyphicon "log-out"

newWindow :: DOMElement
newWindow = glyphicon "new-window"

record :: DOMElement
record = glyphicon "record"

save :: DOMElement
save = glyphicon "save"

open :: DOMElement
open = glyphicon "open"

saved :: DOMElement
saved = glyphicon "saved"

import' :: DOMElement
import' = glyphicon "import"

export :: DOMElement
export = glyphicon "export"

send :: DOMElement
send = glyphicon "send"

floppyDisk :: DOMElement
floppyDisk = glyphicon "floppy-disk"

floppySaved :: DOMElement
floppySaved = glyphicon "floppy-saved"

floppyRemove :: DOMElement
floppyRemove = glyphicon "floppy-remove"

floppySave :: DOMElement
floppySave = glyphicon "floppy-save"

floppyOpen :: DOMElement
floppyOpen = glyphicon "floppy-open"

creditCard :: DOMElement
creditCard = glyphicon "credit-card"

transfer :: DOMElement
transfer = glyphicon "transfer"

cutlery :: DOMElement
cutlery = glyphicon "cutlery"

header :: DOMElement
header = glyphicon "header"

compressed :: DOMElement
compressed = glyphicon "compressed"

earphone :: DOMElement
earphone = glyphicon "earphone"

phoneAlt :: DOMElement
phoneAlt = glyphicon "phone-alt"

tower :: DOMElement
tower = glyphicon "tower"

stats :: DOMElement
stats = glyphicon "stats"

sdVideo :: DOMElement
sdVideo = glyphicon "sd-video"

hdVideo :: DOMElement
hdVideo = glyphicon "hd-video"

subtitles :: DOMElement
subtitles = glyphicon "subtitles"

soundStereo :: DOMElement
soundStereo = glyphicon "sound-stereo"

soundDolby :: DOMElement
soundDolby = glyphicon "sound-dolby"

sound51 :: DOMElement
sound51 = glyphicon "sound-5-1"

sound61 :: DOMElement
sound61 = glyphicon "sound-6-1"

sound71 :: DOMElement
sound71 = glyphicon "sound-7-1"

copyrightMark :: DOMElement
copyrightMark = glyphicon "copyright-mark"

registrationMark :: DOMElement
registrationMark = glyphicon "registration-mark"

cloudDownload :: DOMElement
cloudDownload = glyphicon "cloud-download"

cloudUpload :: DOMElement
cloudUpload = glyphicon "cloud-upload"

treeConifer :: DOMElement
treeConifer = glyphicon "tree-conifer"

treeDeciduous :: DOMElement
treeDeciduous = glyphicon "tree-deciduous"

piggyBank :: DOMElement
piggyBank = glyphicon "piggy-bank"
