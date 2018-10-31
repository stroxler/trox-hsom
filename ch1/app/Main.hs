module Main where

import Lib (
  Command (PlayNote, PlayLick),
  executeCommand,
  )

import  Options.Applicative (
  -- core types
  Parser, ParserInfo,
  -- bottom-velvel readers
  str, auto,
  -- bottom-level parsers
  option, argument, switch,
  -- modifiers for low-level parsers
  metavar, help, value, short, long,
  -- modifiers for high-level parsers
  progDesc, helper,
  -- converting parser into ParserInfo, then and IO
  info, customExecParser, --  execParser,
  -- handling variant types
  subparser, command,
  -- modifying the top-level ParserInfo behavior
  prefs, showHelpOnError,
  -- not sure what these are - we actually don't use them
  -- in this particular app
  ---- (<**>), header, fullDesc,
  )
-- notes on Options.Applicative basics:
--
-- argument :: ReadM a -> Mod ArgumentFields a -> Parser a
-- option :: ReadM a -> Mod OptionFields a -> Parser a
--
-- str :: Data.String.IsString s => ReadM s
-- auto :: Read a -> ReadM a
-- metavar :: HasMetavar f -> String -> Mod f a
--
-- value :: HasValue f => a -> Mod f a     -- (default value)
-- help :: String -> Mod f a
-- long :: HasName f => String -> Mod f a
-- short :: HasName f => Char -> Mod f a
--   ... also, Mod has a monoid instance
--
-- info :: Parser a -> InfoMod a -> InfoParser a
-- helper :: Parser (a -> a)
-- progDescr :: String -> InfoMod a
--
-- command :: String -> ParserInfo a -> Mod CommandFields a
-- subparser :: Mod CommandFields a -> Parser a
--   ... remember, Mod has a monoid instance
--
-- execParser :: ParserInfo a -> IO a
--
-- showHelpOnError :: PrefsMod
-- prefs :: PrefsMod -> ParserPrefs
-- customExecParser :: ParserPrefs -> ParserInfo a -> IO a




-- a required, positional argument of str type
-- note that the type signature is mandatory
noteArg :: Parser String
noteArg = argument str $
  value "c" <>
  metavar "NOTENAME" <>
  help "Name of note to play (e.g. c, d, e)"

-- an optional argument of int type
-- note that the type signature is mandatory
octaveOpt :: Parser Int
octaveOpt = option auto $
  value 4 <>
  short 'o' <>
  long "octave" <>
  metavar "OCTAVE" <>
  help "Octave of note to play (e.g. 4)"

-- note that we don't actually *need* a type signature here
extendedFlag :: Parser Bool
extendedFlag = switch $
  short 'e' <>
  long "extended" <>
  help "Play extended version of the lick"


-- define the PlayNote version of our parser
noteOptions :: ParserInfo Command
noteOptions = info
  (helper <*> (PlayNote <$> noteArg <*> octaveOpt))
  (progDesc "Play a note")

-- define the PlayLick version of our parser
lickOptions :: ParserInfo Command
lickOptions = info
  (helper <*> (PlayLick <$> extendedFlag))
  (progDesc "Play a lick")

-- combine the parsers for the two Command variants.
-- This is like click or fire subcommands
parser :: Parser Command
parser = subparser
  ( command "note" noteOptions <>
    command "lick" lickOptions )

-- turn the Parser into a ParserInfo
-- (have to do this before we exec; we can use mempty for the Mod
--  if the parser is so simple that there's no use in adding information)
infoParser :: ParserInfo Command
infoParser = info (helper <*> parser) (progDesc "Play a note or a lick")

-- create a customized version of execParser
execWithHelp :: ParserInfo a -> IO a
execWithHelp =
  customExecParser (prefs showHelpOnError)

-- run the parser, then call into Lib to execute our Command
main :: IO ()
main = do
  cmd <- execWithHelp infoParser
  executeCommand cmd
