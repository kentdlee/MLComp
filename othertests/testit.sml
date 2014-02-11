fun cprint s = 
  (TextIO.output(TextIO.stdOut, s);
   cprint)


