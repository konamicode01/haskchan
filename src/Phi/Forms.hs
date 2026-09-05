{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Phi.Forms where

import           Data.Char (ord, toLower)
import           Data.List (find, nub)
import           Data.Maybe (fromJust, isJust)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text as T (all, breakOn, drop, null, pack, replace, splitAt, strip, unpack)
import           Data.Password.Argon2
import           Lucid (Html, renderText, toHtml, toHtmlRaw)
import           Text.RE.PCRE.ByteString
import           Text.RE.Replace (captureText, CaptureName(CaptureName), CaptureID(IsCaptureName))

import           Phi.Database.Models

data Validation a
  = Valid a        -- All forms field that can be checked without IO are valid.
  | Invalid [Text] -- Some form fields were invalid.
  | Aborted        -- Form validation was aborted.
  deriving (Eq, Show)

data Conformity
  = Good        -- The field is valid.
  | Bad Text    -- The field is invalid and has produced an error message.
  | Abort       -- The field is so invalid that form validation is being aborted.
  deriving (Eq, Show)

type Rule = Conformity
type Rulechain = [Rule]
type Form spoils = ([Rulechain], spoils)

type Quotes = ([Int], [Text], [(Text, Int)])

formatMessage :: Text -> (Html (), Quotes)
formatMessage message_ =
  (toHtmlRaw formatted, quotes)
  where
    formatted = decodeUtf8 $ foldr ($) escaped [hyperlink, monospace, replaceLF, removeCR, underline, stricken, italic, bold, titletext, spoiler, pinktext, greentext, postquote, boardquote, foreignquote]
    escaped = encodeUtf8 . TL.toStrict . renderText . toHtml $ message_
    replaceLF = encodeUtf8 . T.replace "\n" "<br>" . decodeUtf8
    removeCR  = encodeUtf8 . T.replace "\r" "" . decodeUtf8
    hyperlink = (*=~/ [edMultilineInsensitive|${url}(\bhttps?://(?:[a-z0-9-]+\.)+[a-z0-9-]+(?:[/?#][a-z0-9-._~%+?&=:;#!@$*()/]*)?)///<a rel="noreferrer" href="${url}">${url}</a>|])
    monospace = (*=~/ [ed|(?<=<pre>)<br>|<br>(?=</pre>)///|]) . (*=~/ [ed|```${text}(.+?)```///<pre>${text}</pre>|])
    underline = (*=~/ [ed|__${text}(.+?)__///<u>${text}</u>|])
    stricken  = (*=~/ [ed|~~${text}(.+?)~~///<s>${text}</s>|])
    italic    = (*=~/ [ed|&#39;&#39;${text}(.+?)&#39;&#39;///<i>${text}</i>|])
    bold      = (*=~/ [ed|&#39;&#39;&#39;${text}(.+?)&#39;&#39;&#39;///<b>${text}</b>|])
    titletext = (*=~/ [ed|==${text}(.+?)==///<span class="titletext">${text}</span>|])
    spoiler   = (*=~/ [ed|\|\|${text}(.+?)\|\|///<span class="spoiler">${text}</span>|])
    pinktext  = (*=~/ [ed|^&lt;${text}(.+)///<span class="pinktext">&lt;${text}</span>|])
    greentext = (*=~/ [ed|^&gt;${text}(.+)///<span class="greentext">&gt;${text}</span>|])
    postquote    = (*=~/ [ed|(?<!\S)&gt;&gt;${no}([1-9][0-9]{0,17})(?!\w)///<a class="quote">&gt;&gt;${1}</a>|])
    boardquote   = (*=~/ [ed|(?<!\S)&gt;&gt;&gt;/${uri}([a-z0-9]{1,32})/(?!\w)///<a class="boardquote">&gt;&gt;&gt;/${1}/</a>|])
    foreignquote = (*=~/ [ed|(?<!\S)&gt;&gt;&gt;/${uri}([a-z0-9]{1,32})/${no}([1-9][0-9]{0,17})(?!\w)///<a class="foreignquote">&gt;&gt;&gt;/${1}/${2}</a>|])

    quotes = (postquotes, boardquotes, foreignquotes)
    group groupname = T.unpack . decodeUtf8 . captureText (IsCaptureName $ CaptureName groupname)

    postquotes =
      nub
      . map (read . group "no")
      . allMatches
      $ escaped *=~ [re|&gt;&gt;${no}([1-9][0-9]{0,17})|]

    boardquotes =
      nub
      . map (T.pack . group "uri")
      . allMatches
      $ escaped *=~ [re|&gt;&gt;&gt;/${uri}([a-z0-9]{1,32})/(?![0-9])|]

    foreignquotes =
      nub
      . map (\match -> (T.pack . group "uri" $ match, read . group "no" $ match))
      . allMatches
      $ escaped *=~ [re|&gt;&gt;&gt;/${uri}([a-z0-9]{1,32})/${no}([1-9][0-9]{0,17})|]

fromBool :: Text -> Bool -> Conformity
fromBool _   True  = Good
fromBool msg False = Bad msg

fromBoolAbort :: Bool -> Conformity
fromBoolAbort True  = Good
fromBoolAbort False = Abort

fromPasswordCheck :: Text -> PasswordCheck -> Conformity
fromPasswordCheck msg pwc = fromBool msg (pwc == PasswordCheckSuccess)

minlength :: Int -> Text -> Text -> Conformity
minlength n text msg = fromBool msg $ not . T.null . T.drop (n - 1) $ text

maxlength :: Int -> Text -> Text -> Conformity
maxlength n text msg = fromBool msg $ T.null . T.drop n $ text

equal :: Text -> Text -> Text -> Conformity
equal x y msg = fromBool msg $ x == y

positive :: Int -> Text -> Conformity
positive n msg = fromBool msg (n > 0)

positif :: Int -> Text -> Conformity
positif n msg = fromBool msg (n >= 0)

largest :: Int -> Int -> Text -> Conformity
largest limit n msg = fromBool msg (n <= limit)

increasing :: Ord a => [a] -> Text -> Conformity
increasing (x:y:zs) msg
  | y >= x    = increasing (y:zs) msg
  | otherwise = Bad msg
increasing _ _msg = Good

alphanumeric :: Text -> Text -> Conformity
alphanumeric text msg = fromBool msg $ T.all (\char -> inrange $ ord char) $ text
  where inrange n = n >= 48 && n < 58 || n >= 65 && n < 90 || n >= 97 && n < 123

oneof :: Text -> [Text] -> Text -> Conformity
oneof x xs msg = fromBool msg $ x `elem` xs

lowercase :: Text -> Text -> Conformity
lowercase text msg = fromBool msg $ T.all (\char -> char == toLower char) $ text

minlength' :: Int -> Text -> Conformity
minlength' n text = fromBoolAbort $ not . T.null . T.drop (n - 1) $ text

maxlength' :: Int -> Text -> Conformity
maxlength' n text = fromBoolAbort $ T.null . T.drop n $ text

positive' :: Int -> Conformity
positive' n = fromBoolAbort (n > 0)

alphanumeric' :: Text -> Conformity
alphanumeric' text = fromBoolAbort $ T.all (\char -> inrange $ ord char) $ text
  where inrange n = n >= 48 && n < 58 || n >= 65 && n < 90 || n >= 97 && n < 123

lowercase' :: Text -> Conformity
lowercase' text = fromBoolAbort $ T.all (\char -> char == toLower char) $ text

validateRC :: Rulechain -> Conformity
validateRC rulechain =
  case find (/= Good) rulechain of
    Just conformity -> conformity
    Nothing         -> Good

unwrap :: [Conformity] -> Maybe [Text]
unwrap []             = Just []
unwrap (Abort : _)    = Nothing
unwrap (Bad msg : cs) = Just msg >>= \msg' -> (msg' :) <$> unwrap cs
unwrap (Good : cs)    = unwrap cs

validate :: Form spoils -> Validation spoils
validate (rulechains, spoils) = do
  let conformities = map validateRC rulechains
  case unwrap conformities of
    Just []       -> Valid spoils
    Just messages -> Invalid messages
    Nothing       -> Aborted

postForm :: Text -> Maybe Int -> Text -> Text -> Text -> Text -> Form NewPost
postForm uri_ mThreadNo name_ email_ subject_ message_ =
  ([uriRC, threadNoRC, nameRC, emailRC, subjectRC, messageRC], newpost)
  where
    uriRC =
      [ minlength  1 uri_ "Board URI was empty"
      , maxlength 32 uri_ "Board URI was longer than 32 characters"
      , alphanumeric uri_ "Board URI was not alphanumeric"
      , lowercase    uri_ "Board URI was not lowercase"
      ]
    threadNoRC =
      [ okThreadNo
      ]
    nameRC =
      [ maxlength 32 name_ "Name was longer than 32 characters"
      ]
    emailRC =
      [ maxlength 32 email_ "Email was longer than 32 characters"
      ]
    subjectRC =
      [ maxlength 64 subject_ "Subject was longer than 64 characters"
      ]
    messageRC =
      [ minlength    1 message_ "Message was empty"
      , maxlength 4096 message_ "Message was longer than 4096 characters"
      ]
    okThreadNo =
      case mThreadNo of
        Nothing        -> Good
        Just threadNo -> positive threadNo "No such thread"
    newpost = NewPost
      { npName     = T.strip brokenName
      , npHashtext = hashtext
      , npEmail    = email_
      , npSubject  = subject_
      , npNomarkup = message_
      , npMessage  = formatted
      , npQuotes   = quotes
      }
    (brokenName, hashtext) =
      T.breakOn "#" name_
    (formatted, quotes) =
      formatMessage message_

registerForm :: Text -> Text -> Text -> Form NewUser
registerForm username_ password passwordAgain =
  ([usernameRC, passwordRC], newuser)
  where
    usernameRC =
      [ minlength    1 username_     "Username was empty"
      , maxlength   32 username_     "Username was longer than 32 characters"
      , alphanumeric   username_     "Username was not alphanumeric"
      ]
    passwordRC =
      [ minlength    8 password      "Password was shorter than 8 characters"
      , maxlength 1024 password      "Password was longer than 1024 characters"
      , maxlength 1024 passwordAgain "Password confirmation was longer than 1024 characters"
      , equal password passwordAgain "Passwords did not match"
      ]
    newuser = NewUser
      { nuUsername = username_
      , nuPassword = mkPassword password
      }

loginForm :: Text -> Text -> Form ()
loginForm username_ password =
  ([usernameRC, passwordRC], ())
  where
    usernameRC =
      [ minlength    1 username_ "Username was empty"
      , maxlength   32 username_ "Username was longer than 32 characters"
      , alphanumeric   username_ "Username was not alphanumeric"
      ]
    passwordRC =
      [ minlength    8 password  "Password was shorter than 8 characters"
      , maxlength 1024 password  "Password was longer than 1024 characters"
      ]

makeBoardForm :: Text -> Text -> Text -> Form NewBoard
makeBoardForm uri_ title_ description_ =
  ([uriRC, titleRC, descriptionRC], newboard)
  where
    uriRC =
      [ minlength  1 uri_ "URI was empty"
      , maxlength 32 uri_ "URI was longer than 32 characters"
      , alphanumeric uri_ "URI was not alphanumeric"
      , lowercase    uri_ "URI was not lowercase"
      ]
    titleRC =
      [ minlength  1 title_ "Title was empty"
      , maxlength 32 title_ "Title was longer than 32 characters"
      ]
    descriptionRC =
      [ maxlength 128 description_ "Description was longer than 128 characters"
      ]
    newboard = NewBoard
      { nbUri         = uri_
      , nbTitle       = title_
      , nbDescription = description_
      }

boardSettingsForm :: Text -> Text -> Text -> Maybe Theme -> Text -> Int -> Int -> Int -> BoardPermission -> IndexViewPolicy -> Text -> Text -> [Text] -> Form BoardSettings
boardSettingsForm uri_ title_ description_ mTheme_ anonName_ bumpLimit_ replyLimit_ threadLimit_ permission_ indexViewPolicy_ addMod untoMods selectMods =
  ([uriRC, titleRC, descriptionRC, anonNameRC, bumpLimitRC, replyLimitRC, threadLimitRC, limitsRC, addModRC, untoModsRC, selectModsRC], boardsettings)
  where
    uriRC =
      [ minlength'  1 uri_
      , maxlength' 32 uri_
      , alphanumeric' uri_
      , lowercase'    uri_
      ]
    titleRC =
      [ minlength  1 title_ "Title was empty"
      , maxlength 32 title_ "Title was longer than 32 characters"
      ]
    descriptionRC =
      [ maxlength 128 description_ "Description was longer than 128 characters"
      ]
    anonNameRC =
      [ minlength  1 anonName_ "Anon name was empty"
      , maxlength 32 anonName_ "Anon name was longer than 32 characters"
      ]
    bumpLimitRC =
      [ positive bumpLimit_ "Bump limit wasn't positive"
      ]
    replyLimitRC =
      [ positive replyLimit_ "Reply limit wasn't positive"
      ]
    threadLimitRC =
      [ positive threadLimit_ "Thread limit wasn't positive"
      ]
    limitsRC =
      [ increasing [bumpLimit_, replyLimit_] "Reply limit was smaller than bump limit"
      ]
    addModRC =
      [ maxlength 32 addMod "New moderator's name was longer than 32 characters"
      , alphanumeric addMod "New moderator's name was not alphanumeric"
      ]
    untoModsRC =
      [ fromBool "Invalid action to perform unto mods" $ isJust mBsUntoMods
      ]
    selectModsRC =
         map (\modname -> maxlength 32 modname "Removed moderator's name was longer than 32 characters") selectMods
      ++ map (\modname -> alphanumeric modname "Removed moderator's name was not alphanumeric") selectMods
    mBsUntoMods =
      case untoMods of
        "remove"  -> Just Nothing
        "promote" -> Just $ Just True
        "demote"  -> Just $ Just False
        _         -> Nothing
    boardsettings = BoardSettings
      { bsTitle           = title_
      , bsDescription     = description_
      , bsMTheme          = mTheme_
      , bsAnonName        = anonName_
      , bsBumpLimit       = bumpLimit_
      , bsReplyLimit      = replyLimit_
      , bsThreadLimit     = threadLimit_
      , bsPermission      = permission_
      , bsIndexViewPolicy = indexViewPolicy_
      , bsAddMod          = if T.null addMod then Nothing else Just addMod
      , bsUntoMods        = fromJust $ mBsUntoMods
      , bsSelectMods      = selectMods
      }

globalSettingsForm :: Theme -> Bool -> Bool -> Bool -> CaptchaProvider -> Form GlobalSettings
globalSettingsForm globalTheme_ openRegistration_ userBoardCreation_ captchaBaseline_ captchaProvider_ =
  ([], globalsettings)
  where
    globalsettings = GlobalSettings
      { globalTheme       = globalTheme_
      , openRegistration  = openRegistration_
      , userBoardCreation = userBoardCreation_
      , captchaBaseline   = captchaBaseline_
      , captchaProvider   = captchaProvider_
      }

modStickinessForm :: Int -> Text -> Form ()
modStickinessForm stickiness_ reason =
  ([stickinessRC, reasonRC], ())
  where
    stickinessRC =
      [ positif      stickiness_ "Stickiness was negative"
      , largest 1024 stickiness_ "Stickiness was greater than 1024"
      ]
    reasonRC =
      [ maxlength 32 reason "Reason was longer than 32 characters"
      ]

modForm :: Text -> Maybe Int -> Maybe Bool -> Text -> [Text] -> Form (ModAction, [(Text, Int)])
modForm modActionName mStickiness mBoolean reason postStrings =
  ([modActionNameRC, stickinessRC, booleanRC, reasonRC, postStringsRC], (modaction, postTuples))
  where
    modActionNameRC =
      [ oneof modActionName modActionNames "No such action"
      ]
    stickinessRC =
      case mStickiness of
        Just stickiness_ ->
          [ positif      stickiness_ "Stickiness was negative"
          , largest 1024 stickiness_ "Stickiness was greater than 1024"
          ]
        Nothing ->
          [ if modActionName == "sticky"
            then Bad "Stickiness was missing"
            else Good
          ]
    booleanRC =
      case mBoolean of
        Just _  -> []
        Nothing ->
          [ if modActionName `elem` ["cycle", "lock", "bumplock"]
            then Bad "Enable / Disable weren't selected"
            else Good
          ]
    reasonRC =
      [ maxlength 32 reason "Reason was longer than 32 characters"
      ]
    postStringsRC =
      postStringsNonempty : postStringsValid

    postStringsNonempty = if null postStrings then Bad "No posts were selected" else Good
    postStringsValid =
      [ if T.all lowalnum uri_ && hyphen == "-" && T.all digit no_
        then Good
        else Bad $ "Invalid post: " <> postString
      | postString <- postStrings
      , let (uri_, noWithHyphen) = T.breakOn "-" postString
            (hyphen, no_) = T.splitAt 1 noWithHyphen
      ]
    lowalnum char = ord char >= 97 && ord char < 123 || digit char
    digit char = ord char >= 48 && ord char < 58

    modActionNames = ["sticky", "cycle", "lock", "bumplock", "unlink", "purge", "delete"]
    modaction = case modActionName of
      "sticky"   -> Sticky     reason $ fromJust mStickiness
      "cycle"    -> Cycle      reason $ fromJust mBoolean
      "lock"     -> Lock       reason $ fromJust mBoolean
      "bumplock" -> Bumplock   reason $ fromJust mBoolean
      "unlink"   -> UnlinkFile reason
      "purge"    -> PurgeFile  reason
      "delete"   -> Delete     reason

    postTuples =
      [ (uri_, no_)
      | postString <- postStrings
      , let (uri_, noWithHyphen) = T.breakOn "-" postString
            no_ = read $ T.unpack $ T.drop 1 noWithHyphen :: Int
      ]

-- Not a form but the same process can be used to validate segments in the URL.
baseBoardPage :: Text -> Maybe Int -> Form ()
baseBoardPage uri_ mCounter =
  ([rc], ())
  where
    rc =
      [ minlength'  1 uri_
      , maxlength' 32 uri_
      , alphanumeric' uri_
      , okCounter
      ]
    okCounter =
      case mCounter of
        Nothing      -> Good
        Just counter -> positive' counter

baseBoardPageNoCounter :: Text -> Form ()
baseBoardPageNoCounter = \uri_ -> baseBoardPage uri_ Nothing

implicitBoardPage :: Text -> Form ()
implicitBoardPage = baseBoardPageNoCounter

cataloguePage :: Text -> Form ()
cataloguePage = baseBoardPageNoCounter

indexPage :: Text -> Int -> Form ()
indexPage uri_ pageInc = baseBoardPage uri_ (Just pageInc)

threadPage :: Text -> Int -> Form ()
threadPage uri_ no_ = baseBoardPage uri_ (Just no_)

boardSettingsPage :: Text -> Form ()
boardSettingsPage uri_ =
  ([rc], ())
  where
    rc =
      [ minlength'  1 uri_
      , maxlength' 32 uri_
      , alphanumeric' uri_
      ]
