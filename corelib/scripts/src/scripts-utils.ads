
with GNAT.Strings;

package Scripts.Utils is

   function Argument_List_To_Quoted_String
     (Args            : GNAT.Strings.String_List;
      Quote           : Character := '"';
      Quote_Backslash : Boolean := True) return String;
   --  Return the arguments as a full string.
   --  Arguments that contain spaces but do not already contain quotes
   --  will be put into quotes.
   --  Backslashes are duplicated if Quote_Baskslash is True.
   --  The result of this subprogram on the string     A simple\ "string"
   --  is:     Quote_Backslash =>   "A simple\\ \"string\""
   --      not Quote_Backslash =>   "A simple\ \"string\""

   function Argument_String_To_List_With_Triple_Quotes
     (Arg_String : String) return GNAT.Strings.String_List_Access;
   --  This is similar to GNAT.OS_Lib.Argument_String_To_List, except that
   --  if part of the string is surrounded by triple quotes, any special
   --  character is ignored till the closing triple quotes. This is the same
   --  behavior as in Python, and is needed for easier quoting of string.
   --
   --  Here is the output in some cases:
   --     "foo"       -> "foo"       (quotes preserved)
   --     """foo"""   -> foo         (quotes removed when at beginning and end)
   --     ("""foo""") -> ("""foo""") (quotes preserved in middle)
   --     foo\"foo    -> foo\"foo    (backslash not removed from output)

   function Unprotect (Str : String) return String;
   --  Remove the \ protections in Str

   function Read_File (File : String) return GNAT.Strings.String_Access;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  File is a UTF8-encoded string

end Scripts.Utils;
