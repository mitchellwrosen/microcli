{-# LANGUAGE DataKinds, DeriveFunctor, DerivingStrategies, ExplicitNamespaces,
             GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase,
             OverloadedStrings, PartialTypeSignatures, ScopedTypeVariables,
             TypeFamilies, TypeOperators, UnicodeSyntax,
             ViewPatterns #-}

module Options.Micro
  ( -- * Parser
    Parser
  , makeParser
  , runParser
    -- ** Option parser
  , OptParser
  , positional
  , named
  , switch
  , opt
  , oneof
    -- ** Argument parser
    -- ** Command parser
  , CmdParser
  , Command(..)
  , commands
  , noCommands
    -- ** Value parser
  , ValParser(..)
  , string
  , text
  , rational
  , decimal
    -- * Types
  , Default(..)
  , Description(..)
  , Help(..)
  , Metavar(..)
  , Name(..)
  , ShowDefault(..)
    -- * Notes
    -- $note
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


-- | A command-line parser.
--
-- Construct with 'makeParser' and run with 'runParser'.
newtype Parser a
  = Parser (O.Parser a)
  deriving newtype (Functor)

-- | Construct a parser from:
--
-- * An /option/ parser, which parses zero or more named options, e.g. @\'--quiet\'@
--   and @\'-n 1\'@.
--
-- * An /argument/ parser, which parses zero or more unnamed positional arguments,
--   e.g. @\'localhost\'@.
--
-- * A /subcommand/ parser, which defines zero or more named subcommand parsers,
--   for e.g. the @\'clone\'@ command in @\'git clone\'@.
makeParser
  :: (a -> b -> c)
  -> OptParser a -- ^
  -> CmdParser b -- ^
  -> Parser c
makeParser f (OptParser p) (CmdParser q) =
  Parser (f <$> p <*> q)

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


-- | A command-line option parser.
--
-- * Use 'opt' to parse a single option.
-- * Use ('<*>') to parse two (or more) different options.
newtype OptParser a
  = OptParser (O.Parser a)
  deriving newtype (Applicative, Functor)

data Named = Named | Unnamed

data Optional = Optional | Required

data Option (named :: Named) (optional :: Optional) a where
  OptPositional
    :: (Text -> Either Text a)
    -> Metavar
    -> Help
    -> Option 'Unnamed 'Required a

  OptNamed
    :: (Text -> Either Text a)
    -> Name
    -> Metavar
    -> Help
    -> Default optional a
    -> ShowDefault optional a
    -> Option 'Named optional a

  OptSwitch
    :: a
    -> Name
    -> Help
    -> Default optional a
    -> Option 'Named optional a

positional
  :: (Text -> Either Text a)
  -> Metavar
  -> Help
  -> Option 'Unnamed 'Required a
positional =
  OptPositional

-- | Construct an named option.
--
-- === __Examples__
--
-- * Construct a parser of @\'-t/--threads \<int\>\'@ with a default value of 1.
--
--     @
--     threadsParser :: 'OptParser' Int
--     threadsParser =
--       'opt'
--         ('ShortLong' \'t\' \"threads\")
--         ('Help' \"Number of threads.\")
--         ('Default' 1)
--         ('Attempt' ('Metavar' \"THREADS\") 'decimal')
--     @
--
-- * Construct a simple "switch" parser, which succeeds with 'True' if present,
--   and succeeds with 'False' if absent.
--
--     @
--     quietParser :: 'OptParser' Bool
--     quietParser =
--       'opt'
--         ('ShortLong' \'q\' \"quiet\")
--         ('Help' \"Suppress output.\")
--         ('Default' False)
--         ('Succeed' True)
--     @
named
  :: (Text -> Either Text a)
  -> Name
  -> Metavar
  -> Help
  -> Default optional a
  -> ShowDefault optional a
  -> Option 'Named optional a
named =
  OptNamed

-- | Construct a switch.
switch
  :: a
  -> Name
  -> Help
  -> Default optional a
  -> Option 'Named optional a
switch =
  OptSwitch

-- | Construct an option parser from a single option.
opt :: Option named optional a -> OptParser a
opt =
  coerce opt_

opt_ :: Option named optional a -> O.Parser a
opt_ = \case
  OptPositional a b c ->
    optPositional a b c

  OptNamed a b c d e f ->
    optNamed a b c d e f

  OptSwitch a b c d ->
    optSwitch a b c d

optNamed
  :: (Text -> Either Text a) -- ^ Parse function to run, if present
  -> Name                    -- ^ Option name (short, long, or both)
  -> Metavar                 -- ^ Metavariable, shown in @--help@ output
  -> Help                    -- ^ Help text, shown in @--help@ output
  -> Default optional a      -- ^ Default value, if missing
  -> ShowDefault optional a
  -> O.Parser a
optNamed (makeReadM -> read) name metavar help def showDef =
  case def of
    NoDefault ->
      O.option read
        (mconcat
          [ nameMod name
          , helpMod help
          , metavarMod metavar
          ])

    Default def' ->
      case showDef of
        ShowDefault showDef' ->
          O.option read
            (mconcat
              [ nameMod name
              , helpMod help
              , metavarMod metavar
              , O.value def'
              , O.showDefaultWith showDef'
              ])
          <|>
          pure def'

optSwitch
  :: a -- ^
  -> Name -- ^
  -> Help -- ^
  -> Default optional a -- ^
  -> O.Parser a
optSwitch val name help def =
  O.flag' val
    (mconcat
      [ nameMod name
      , helpMod help
      ])
  <|>
  case def of
    NoDefault    -> empty
    Default def' -> pure def'

oneof
  :: [Option 'Named 'Required a]
  -> OptParser a
oneof =
  OptParser . asum . map opt_


-- | Parse a positional argument.
--
-- ==== __Examples__
--
-- @
-- -- Construct a parser of \"\<int\>\".
--
-- intParser :: 'ArgParser' Int
-- intParser =
--   'arg'
--     ('Metavar' \"INT\")
--     ('Help' \"An int.\")
--     'decimal'
-- @
optPositional
  :: (Text -> Either Text a) -- ^ Parse function to run
  -> Metavar -- ^ Metavariable name, shown in @--help@ output
  -> Help -- ^ Help text, shown in @--help@ output
  -> O.Parser a
optPositional (makeReadM -> read) metavar help =
  O.argument read
    (mconcat
      [ helpMod help
      , metavarMod metavar
      ])


-- | A command-line command parser.
newtype CmdParser a
  = CmdParser (O.Parser a)
  deriving newtype (Functor)

-- | A command is a named sub-parser, like @clone@ in @git clone@.
data Command a where
  Command      :: Text -> Description -> Parser a -> Command a
  CommandGroup :: Text -> Command a

-- | Construct a parser from a list of command parsers.
--
-- Use the 'CommandGroup' header to group the subsequent commands in the
-- @--help@ output.
commands :: [Command a] -> CmdParser a
commands =
  CmdParser . asum . map g . ($ []) . snd . foldl' f (Nothing, id)
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
  f (group, acc) command =
    case command of
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

noCommands :: CmdParser ()
noCommands =
  CmdParser (pure ())


-- | A value parser:
--
-- * __@Succeed@__ Do not expect any string to parse, just succeed with the
--   given value.
--
-- * __@Attempt@__ Parse a string with the given function, and provide a
--   metavariable for this string in the @--help@ output.
data ValParser a
  = Succeed a
  | Attempt Metavar (Text -> Either Text a)
  deriving stock (Functor)

string :: Text -> Either Text String
string =
  Right . Text.unpack

text :: Text -> Either Text Text
text =
  Right

-- | Parse a decimal.
decimal :: Integral a => Text -> Either Text a
decimal s =
  case Text.signed Text.decimal s of
    Right (x, "") -> Right x
    _             -> Left ("`" <> s <> "` is not a decimal")

-- | Parse a rational number.
rational :: Fractional a => Text -> Either Text a
rational s =
  case Text.signed Text.rational s of
    Right (x, "") -> Right x
    _             -> Left ("`" <> s <> "` is not a rational number")


-- | Default value to provide if an option is missing. If an option is provided
-- but fails to parse, the default value is /not/ used.
--
-- For example, you may wish to parse an optional @--threads@ as an 'Int', with
-- a default value of @1@:
--
-- @
-- 'opt'
--   ('Long' "threads")
--   ('Help' "Number of threads")
--   ('Default' 1)
--   ('Attempt' ('Metavar' "N") 'decimal')
-- @
--
-- This parser will:
--
-- * Succeed with @1@ if @--threads@ is missing
-- * Succeed with @4@ if @--threads 4@ is provided
-- * Fail if @--threads foo@ is provided
data Default (optional :: Optional) a where
  NoDefault
    :: Default 'Required a

  Default
    :: a
    -> Default 'Optional a

data ShowDefault (optional :: Optional) a where
  NoShowDefault
    :: ShowDefault 'Required a

  ShowDefault
    :: (a -> String)
    -> ShowDefault 'Optional a

-- | Program description, shown in @--help@ output.
data Description
  = Description Text

-- | Help text, shown in @--help@ output.
newtype Help
  = Help Text

-- | Option metavariable, shown in @--help@ output.
newtype Metavar
  = Metavar Text

-- | Option or argument name: short, long, or both.
data Name
  = Short Char
  | Long Text
  | ShortLong Char Text

--------------------------------------------------------------------------------
-- Misc. helpers

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

-- $note
--
-- == @Note [Option parser alternative]@
--
-- '<|>' on 'OptParser' is only concerned with the /structure/ of the
-- command-line arguments; the right alternative will only be tried if the left
-- alternative will clearly fail based only on the options names provided,
-- without attempting to parse any values.
--
-- Furthermore, if the left alternative will /not/ clearly fail (as is the case
-- if either the option's name is provided, the option was built with 'pure', or
-- the option has a default value), then the right alternative will /not/ be
-- tried, even if the left alternative ultimately fails during value parsing.
--
-- In other words, if \"@--threads 5@\", is given on the command line, the
-- existence of \"@--threads@\" is relevant to which parser ultimately runs, but
-- the \"@5@\" is not.
--
-- Here is the only sensible kind of option to use on the left-hand side of
-- '<|>':
--
-- * The left alternative may fail, because its name may be missing from the
--   command line arguments, and it does not have a default value.
--
--     @
--     'opt' name help 'NoDefault' parse '<|>' ...
--     @
--
-- And here are a few examples of nonsense:
--
-- * The left alternative will never fail, because it has a default value.
--
--     @
--     'opt' name help ('Default' 5) parse '<|>' ...
--     @
--
-- * The left alternative will never fail, because it was constructed with
--   'pure'.
--
--     @
--     'pure' 5 '<|>' ...
--     @
--
-- * The left and right alternatives share a name, and the right alternative
--   does not have a default value. No matter what command-line arguments are
--   given, it will never be the case that the left alternative fails while the
--   right alternative does not.
--
--     @
--     'opt' ('Short' \'n\') help1 def1      parse1 '<|>'
--     'opt' ('Short' \'n\') help2 'NoDefault' parse2
--     @
--
-- An early version of this library attempted to rule out these uses with the
-- type system, but I felt too much convenience and simplicity was lost by
-- avoiding the 'Applicative' and 'Alternative' type classes in @base@.
