{-# LANGUAGE DataKinds, DeriveFunctor, DerivingStrategies, ExplicitNamespaces,
             GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase,
             OverloadedStrings, PartialTypeSignatures, ScopedTypeVariables,
             TypeFamilies, TypeOperators, UnicodeSyntax, ViewPatterns #-}

module Options.Micro
  ( -- * Parser
    Parser
  , makeParser
  , runParser
    -- ** Option parser
  , OptParser
  , opt
  , switch
  , req
    -- ** Argument parser
    -- ** Command parser
  , CmdParser
  , Command(..)
  , commands
  , noCommands
    -- ** Value parser
  , ValParser(..)
  , rational
  , decimal
    -- * Types
  , Default(..)
  , Description(..)
  , Help(..)
  , Metavar(..)
  , Name(..)
    -- * Notes
    -- $note
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor      (first)
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
-- * Use ('<|>') to parse one option in one of two (or more) different ways.
--
-- Note that ('<|>') has somewhat unintuitive semantics; please see
-- @Note [Option parser alternative]@ at the bottom of this module for more
-- information.
newtype OptParser a
  = OptParser (O.Parser a)
  deriving newtype (Applicative, Functor)

-- * No name:      Read Text,       Metavar, Help,
-- * Name, val:    Read Text, Name, Metavar, Help, Default
-- * Name, no val:            Name,          Help

data Opt (named :: Bool) a where
  Unnamed
    :: (Text -> Either Text a)
    -> Metavar
    -> Help
    -> Opt 'False a

  Named
    :: (Text -> Either Text a)
    -> Name
    -> Metavar
    -> Help
    -> Default a
    -> Opt 'True a

  Switch
    :: Name
    -> Help
    -> Opt 'True Bool

  Fmap
    :: (a -> b)
    -> Opt named a
    -> Opt named b

instance Functor (Opt named) where
  fmap = Fmap

xarg
  :: (Text -> Either Text a)
  -> Metavar
  -> Help
  -> Opt 'False a
xarg =
  Unnamed

xopt
  :: (Text -> Either Text a)
  -> Name
  -> Metavar
  -> Help
  -> Default a
  -> Opt 'True a
xopt =
  Named

xswitch
  :: Name
  -> Help
  -> Opt 'True Bool
xswitch =
  Switch


-- | Construct an option parser.
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
opt
  :: (Text -> Either Text a) -- ^ Parse function to run, if present
  -> Name                    -- ^ Option name (short, long, or both)
  -> Metavar                 -- ^ Metavariable, shown in @--help@ output
  -> Help                    -- ^ Help text, shown in @--help@ output
  -> Default a               -- ^ Default value, if missing
  -> OptParser a
opt (makeReadM -> read) name metavar help (Default def sho) = OptParser $
  O.option read
    (mconcat
      [ nameMod name
      , helpMod help
      , metavarMod metavar
      , O.value def
      , O.showDefaultWith sho
      ])
  <|>
  pure def

opt2
  :: Opt named a
  -> OptParser a
opt2 = \case
  Unnamed a b c ->
    arg a b c

  Named a b c d e ->
    opt a b c d e

  Switch a b ->
    switch a b

  Fmap f x ->
    opt2 (f <$> x)

switch
  :: Name -- ^
  -> Help -- ^
  -> OptParser Bool
switch name help = OptParser $
  O.switch
    (mconcat
      [ nameMod name
      , helpMod help
      ])

req
  :: [(Name, Help, ValParser a)]
  -> OptParser a
req =
  OptParser . asum . map (uncurry3 req1)


req1
  :: Name -- ^
  -> Help -- ^
  -> ValParser a -- ^
  -> O.Parser a
req1 name help = \case
  Succeed val ->
    (O.flag' val . mconcat)
      [ nameMod name
      , helpMod help
      ]

  Attempt metavar (makeReadM -> read) ->
    (O.option read . mconcat)
      [ nameMod name
      , helpMod help
      , metavarMod metavar
      ]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) =
  f x y z

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
arg
  :: (Text -> Either Text a) -- ^ Parse function to run
  -> Metavar -- ^ Metavariable name, shown in @--help@ output
  -> Help -- ^ Help text, shown in @--help@ output
  -> OptParser a
arg (makeReadM -> read) metavar help = OptParser $
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
data Default a
  = Default a (a -> String)

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