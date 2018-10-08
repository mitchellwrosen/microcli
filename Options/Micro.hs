{-# LANGUAGE DataKinds, DerivingStrategies, GADTs, GeneralizedNewtypeDeriving,
             LambdaCase, OverloadedStrings, TypeFamilies, ViewPatterns #-}

module Options.Micro
  ( -- * Parser
    makeParser
  , runParser
    -- ** Option parser
  , option
    -- *** Options
  , positional
  , named
  , flag
  , repeated
  , alternatives
  , ifMissing
    -- *** Readers
  , chars
  , text
  , rational
  , decimal
    -- ** Command parser
  , commands
  , noCommands
    -- *** Commands
  , command
  , commandGroup
    -- * Types
  , Parser
  , OptionParser
  , Option
  , Named(..)
  , Optional(..)
  , CommandParser
  , Command
  , Description(..)
  , Help(..)
  , Metavar(..)
  , Name(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor      (first)
import           Data.Coerce         (coerce)
import           Data.Foldable       (asum)
import           Data.List           (foldl')
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Read      as Text
import qualified Options.Applicative as O
import           Prelude             hiding (read)


--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- | A command-line option parser.
--
-- Construct with 'makeParser' and run with 'runParser'.
newtype Parser a
  = Parser (O.Parser a)
  deriving newtype (Functor)

-- | Construct a parser from:
--
-- * An option parser, which parses zero or more positional/named options,
-- e.g. @--quiet@ and @-n1@.
--
-- * A subcommand parser, which defines zero or more named subcommand parsers,
--   for e.g. the @clone@ command in @git clone@.
makeParser
  :: OptionParser a -- ^
  -> CommandParser (a -> b) -- ^
  -> Parser b
makeParser (OptionParser p) (CommandParser q) =
  Parser (p <**> q)

-- | Run a parser against the provided command-line arguments.
--
-- If parsing fails, help text will be output to the terminal. Otherwise, the
-- parsed @IO@ action will be run.
runParser
  :: Description -- ^ Program description.
  -> Parser (IO ()) -- ^ Command-line option parser.
  -> IO ()
runParser (Description description) (Parser parser) =
  join
    (O.customExecParser
      (O.ParserPrefs
        { O.prefMultiSuffix     = ""
        , O.prefDisambiguate    = True
        , O.prefShowHelpOnError = True
        , O.prefShowHelpOnEmpty = True
        , O.prefBacktrack       = True
        , O.prefColumns         = 80
        })
      (O.info
        (helper <*> parser)
        (O.fullDesc <> O.progDesc (Text.unpack description))))


--------------------------------------------------------------------------------
-- Option parser
--------------------------------------------------------------------------------

-- | A command-line option parser.
--
-- * Use 'one' to parse a single option.
-- * Use 'oneof' to parse two (or more) alternative options as a single option.
-- * Use ('<*>') to parse two (or more) different options.
newtype OptionParser a
  = OptionParser (O.Parser a)
  deriving newtype (Applicative, Functor)

-- | Construct an option parser from a single option.
option
  :: Option named optional a -- ^
  -> OptionParser a
option =
  coerce option_

option_
  :: Option named optional a -- ^
  -> O.Parser a
option_ = \case
  OptPositional a b c ->
    optPositional a b c

  OptNamed a b c d ->
    optNamed a b c d mempty

  OptFlag a b c ->
    optFlag a b c

  OptRepeated a ->
    many (option_ a)

  OptAsum a ->
    asum (map option_ a)

  OptIfMissing (OptNamed a b c d) def ->
    optNamed a b c d (O.value def <> O.showDefault) <|> pure def

  OptIfMissing (OptFlag a b c) def ->
    optFlag a b c <|> pure def

  OptIfMissing (OptAsum a) def ->
    asum (map option_ a) <|> pure def

optPositional :: (Text -> Either Text a) -> Metavar -> Help -> O.Parser a
optPositional (makeReadM -> read) metavar help =
  O.argument read
    (mconcat
      [ helpMod help
      , metavarMod metavar
      ])

optNamed
  :: Show a
  => (Text -> Either Text a)
  -> Name
  -> Metavar
  -> Help
  -> O.Mod O.OptionFields a
  -> O.Parser a
optNamed (makeReadM -> read) name metavar help mods =
  O.option read (mods <> nameMod name <> helpMod help <> metavarMod metavar)

optFlag :: a -> Name -> Help -> O.Parser a
optFlag val name help =
  O.flag' val
    (mconcat
      [ nameMod name
      , helpMod help
      ])


-- | A command-line option, tagged with whether it is named, and whether it is
-- optional.
--
-- * Use 'positional', 'named', or 'flag' to construct a single option.
-- * Use 'repeated' to parse a named option zero or more times.
-- * Use 'ifMissing' to provide a default value.
data Option (named :: Named) (optional :: Optional) a where
  OptPositional
    :: (Text -> Either Text a)
    -> Metavar
    -> Help
    -> Option 'Unnamed 'Required a

  OptNamed
    :: Show a
    => (Text -> Either Text a)
    -> Name
    -> Metavar
    -> Help
    -> Option 'Named 'Required a

  OptFlag
    :: a
    -> Name
    -> Help
    -> Option 'Named 'Required a

  OptRepeated
    :: Option named 'Required a
    -> Option named 'Optional [a]

  OptAsum
    :: [Option 'Named 'Required a]
    -> Option 'Named 'Required a

  OptIfMissing
    :: Option 'Named 'Required a
    -> a
    -> Option 'Named 'Optional a

-- | Construct a positional option.
--
-- === __Examples__
--
-- * A positional 'Text' option.
--
--     @
--     hostOpt :: 'Option' ''Unnamed' ''Required 'Text'
--     hostOpt =
--       'positional' 'text' ('Metavar' \"HOSTNAME\") ('Help' \"The hostname.\")
--     @
positional
  :: (Text -> Either Text a) -- ^ Parse function to run, if present
  -> Metavar -- ^ Metavariable, shown in @--help@ output
  -> Help -- ^ Help text, shown in @--help@ output
  -> Option 'Unnamed 'Required a
positional =
  OptPositional

-- | Construct an named option.
--
-- === __Examples__
--
-- * A named 'Int' option called @-t@ or @--threads@, with a default value of
--   @1@.
--
--     @
--     threadsOpt :: 'Option' ''Named' ''Optional' 'Int'
--     threadsOpt =
--       'ifMissing'
--         ('named'
--           'decimal'
--           ('ShortLong' \'t\' \"threads\")
--           ('Metavar' \"N\")
--           ('Help' \"Number of threads.\"))
--         1
--     @
--
-- * A named 'Text' option called @--host@ with no default value.
--
--     @
--     hostOpt :: 'Option' ''Named' ''Required' 'Text'
--     hostOpt =
--       'named'
--         'text'
--         ('Long \"host\")
--         ('Metavar' \"HOSTNAME\")
--         ('Help' \"The hostname.\")
--     @
named
  :: Show a
  => (Text -> Either Text a) -- ^ Parse function to run, if present
  -> Name -- ^ Option name (short, long, or both)
  -> Metavar -- ^ Metavariable, shown in @--help@ output
  -> Help -- ^ Help text, shown in @--help@ output
  -> Option 'Named 'Required a
named =
  OptNamed

-- | Construct a flag.
--
-- A flag is like a named option without a value; when the flag's name is
-- encountered, it succeeds with the given value.
--
-- === __Examples__
--
-- * Create a simple boolean switch, using 'flag' and 'ifMissing' together
--
--     @
--     quietOpt :: 'Option' ''Named' ''Optional' 'Bool'
--     quietOpt =
--       'ifMissing'
--         ('flag' 'True' ('Short' \'q\') ('Help' \"Suppress output."))
--         'False'
--     @
flag
  :: a -- ^ Flag value, if present
  -> Name -- ^ Option name (short, long, or both)
  -> Help -- ^ Help text, shown in @--help@ output
  -> Option 'Named 'Required a
flag =
  OptFlag

-- | Repeat an option zero or more times.
repeated
  :: Option named 'Required a -- ^
  -> Option named 'Optional [a]
repeated =
  OptRepeated

-- | Construct an option from one or more alternatives.
alternatives
  :: [Option 'Named 'Required a] -- ^
  -> Option 'Named 'Required a
alternatives =
  OptAsum

-- | Provide a default for a named option, if it's missing.
ifMissing
  :: Option 'Named 'Required a -- ^ Option
  -> a -- ^ Default value
  -> Option 'Named 'Optional a
ifMissing =
  OptIfMissing


--------------------------------------------------------------------------------
-- Readers
--------------------------------------------------------------------------------

-- | Parse a list of 'Char'.
chars :: Text -> Either Text [Char]
chars =
  Right . Text.unpack

-- | Parse a 'Text'.
text :: Text -> Either Text Text
text =
  Right

-- | Parse a decimal.
decimal :: Integral a => Text -> Either Text a
decimal s =
  case Text.signed Text.decimal s of
    Right (x, "") -> Right x
    _             -> Left ("`" <> s <> "' is not a decimal")

-- | Parse a rational number.
rational :: Fractional a => Text -> Either Text a
rational s =
  case Text.signed Text.rational s of
    Right (x, "") -> Right x
    _             -> Left ("`" <> s <> "' is not a rational number")


--------------------------------------------------------------------------------
-- Command parser
--------------------------------------------------------------------------------

-- | A command-line command parser.
newtype CommandParser a
  = CommandParser (O.Parser a)
  deriving newtype (Functor)

-- | Construct a subcommand parser from a list of one or more subcommand
-- parsers.
--
-- Use the pseudo-command 'commandGroup' to provide a name for the subsequent
-- commands, which is shown in @--help@ output.
commands :: [Command a] -> CommandParser a
commands =
  CommandParser . asum . map g . ($ []) . snd . foldl' f (Nothing, id)
 where
  f
    :: ( Maybe String
       , [(Maybe String, Text, Description, O.Parser a)] ->
         [(Maybe String, Text, Description, O.Parser a)]
       )
    -> Command a
    -> ( Maybe String
       , [(Maybe String, Text, Description, O.Parser a)] ->
         [(Maybe String, Text, Description, O.Parser a)]
       )
  f (group, acc) = \case
    Command name description (Parser parser) ->
      (group, acc . ((group, name, description, parser) :))

    CommandGroup group' ->
      (Just (Text.unpack group'), acc)

  g :: (Maybe String, Text, Description, O.Parser a) -> O.Parser a
  g (group, Text.unpack -> name, Description description, parser) =
    helper <*>
      O.subparser
        (mconcat
          [ O.command
              name
              (O.info parser
                (O.fullDesc <> O.progDesc (Text.unpack description)))
          , O.metavar name
          , maybe mempty O.commandGroup group
          ])

-- | Construct a command parser that parses no commands.
noCommands :: CommandParser (a -> a)
noCommands =
  CommandParser (pure id)


-- | A command is a named sub-parser, like @clone@ in @git clone@.
data Command a where
  Command      :: Text -> Description -> Parser a -> Command a
  CommandGroup :: Text -> Command a

-- | Construct a subcommand.
command
  :: Text -- ^ Command name
  -> Description -- ^ Command description, shown in @--help@ output
  -> Parser a -- ^ Command parser, run when the command name is encountered
  -> Command a
command =
  Command

-- | Name a group of subcommands.
commandGroup
  :: Text -- ^ Command group name
  -> Command a
commandGroup =
  CommandGroup

--------------------------------------------------------------------------------
-- Misc. types
--------------------------------------------------------------------------------

-- | Program description, shown in @--help@ output.
data Description
  = Description Text

-- | Help text, shown in @--help@ output.
newtype Help
  = Help Text

-- | Option metavariable, shown in @--help@ output.
newtype Metavar
  = Metavar Text

-- | Option name (short, long, or both).
data Name
  = Short Char
  | Long Text
  | ShortLong Char Text

-- | Whether an option is named.
data Named
  = Named
  | Unnamed

-- | Whether a named option is optional.
data Optional
  = Optional
  | Required


--------------------------------------------------------------------------------
-- Misc. helpers
--------------------------------------------------------------------------------

helper :: O.Parser (a -> a)
helper =
  O.abortOption
    O.ShowHelpText
      (O.long "help" <> O.help "Show this help text" <> O.internal)

makeReadM :: (Text -> Either Text a) -> O.ReadM a
makeReadM read =
  O.eitherReader (first Text.unpack . read . Text.pack)

nameMod :: O.HasName f => Name -> O.Mod f a
nameMod = \case
  Short short          -> O.short short
  Long long            -> O.long (Text.unpack long)
  ShortLong short long -> O.short short <> O.long (Text.unpack long)

helpMod :: Help -> O.Mod f a
helpMod (Help help) =
  O.help (Text.unpack help)

metavarMod :: O.HasMetavar f => Metavar -> O.Mod f a
metavarMod (Metavar metavar) =
  O.metavar (Text.unpack metavar)
