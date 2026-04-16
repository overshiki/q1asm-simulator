module Q1Asm.Repl.Commands
  ( Command(..)
  , InfoTarget(..)
  , parseCommand
  ) where

data Command
  = CmdLoad FilePath
  | CmdNext
  | CmdNextQ1
  | CmdNextRt
  | CmdContinue
  | CmdInfo InfoTarget
  | CmdReset
  | CmdHelp
  | CmdQuit
  | CmdUnknown String
  | CmdEmpty
  deriving (Show)

data InfoTarget
  = InfoQ1 | InfoRt | InfoQueue | InfoBins | InfoWaveforms
  deriving (Show)

parseCommand :: String -> Command
parseCommand line = case words line of
  ["load", path]    -> CmdLoad path
  ["n"]             -> CmdNext
  ["next"]          -> CmdNext
  ["nq1"]           -> CmdNextQ1
  ["next", "q1"]    -> CmdNextQ1
  ["nrt"]           -> CmdNextRt
  ["next", "rt"]    -> CmdNextRt
  ["c"]             -> CmdContinue
  ["continue"]      -> CmdContinue
  ["info", "q1"]    -> CmdInfo InfoQ1
  ["i", "q1"]       -> CmdInfo InfoQ1
  ["info", "rt"]    -> CmdInfo InfoRt
  ["i", "rt"]       -> CmdInfo InfoRt
  ["info", "queue"] -> CmdInfo InfoQueue
  ["i", "q"]        -> CmdInfo InfoQueue
  ["info", "bins"]  -> CmdInfo InfoBins
  ["i", "b"]        -> CmdInfo InfoBins
  ["info", "waveforms"] -> CmdInfo InfoWaveforms
  ["i", "w"]        -> CmdInfo InfoWaveforms
  ["reset"]         -> CmdReset
  ["r"]             -> CmdReset
  ["help"]          -> CmdHelp
  ["h"]             -> CmdHelp
  ["quit"]          -> CmdQuit
  ["q"]             -> CmdQuit
  []                -> CmdEmpty
  ws                -> CmdUnknown (unwords ws)
