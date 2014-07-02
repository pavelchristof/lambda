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
             (  short 'a' 
             <> long "action" 
             <> help "Compiler action." 
             <> metavar "ACTION" 
             <> value Evaluate 
             <> showDefault
             <> completeWith (map show $ enumFrom (minBound :: Action))
             )

main :: IO ()
main = execParser opts >>= runDriver driver
    where opts = info (helper <*> config)
                      (  fullDesc
                      <> progDesc "Lambda interpreter."
                      )
