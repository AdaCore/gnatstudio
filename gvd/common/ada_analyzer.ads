package Source_Analyzer is

   type Casing_Type is (Unchanged, Upper, Lower, Mixed);
   --  Casing used for identifiers and reserved words.

   procedure Format_Ada
     (Buffer           : String;
      Indent_Level     : Natural     := 3;
      Indent_Continue  : Natural     := 2;
      Reserved_Casing  : Casing_Type := Lower;
      Ident_Casing     : Casing_Type := Mixed;
      Format_Operators : Boolean     := True);
   --  Format Buffer and output the result on standard output.
   --  Indent_Level is the number of spaces when indenting a block.
   --  Indent_Continue is the number of spaces for a continuation line.
   --  Reserved_Casing specifies the casing for reserved words.
   --  Ident_Casing specifies the casing for identifiers.
   --  If Format_Operators is True, spaces are added when appropriate around
   --  operators (e.g a space after commas, before left paren, etc...).

end Source_Analyzer;
