--  This package is used to preload text files into the memory.
--  It provides ability for fast getting arbitrary line from that
--  preloaded file.
--  Also it transforms ASCII.HT characters to a sequence of spaces
--  using this rule:

--  with SN; use SN;

package File_Buffer is

   procedure Init (File_Name : String);
   --  Preloads specified file. Exceptions from Ada.Text_IO for
   --  Open/Close procedures can be raised in a case of IO errors.

   function Get_Line (Line : Integer) return String;
   --  Returns specified line from preloaded file.

   procedure Done;
   --  Signals that preloaded text file is not needed any more.

end File_Buffer;
