import Options.Applicative
import Lambda.Driver

config :: Parser Config
config = Config 
     <$> (optional . argument str)
             (  metavar "INPUT FILE"
             )
     <*> (optional . strOption)
             (  short 'o' 
             <> long "output" 
             <> help "Output file." 
             <> metavar "FILE"
             )
     <*> option
             (  short 't' 
             <> long "target" 
             <> help "Compiler target." 
             <> metavar "TARGET" 
             <> value DumpTypedAST 
             <> showDefault
             <> completeWith (map show $ enumFrom (minBound :: Target))
             )

main :: IO ()
main = execParser opts >>= runDriver driver
    where opts = info (helper <*> config)
                      (  fullDesc
                      <> progDesc "Lambda compiler."
                      )
