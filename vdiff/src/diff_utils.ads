package Diff_Utils is

   type Diff_Range is record
      First : Natural;
      Last  : Natural;
   end record;

   type Diff_Action is (Append, Change, Delete);

   type Diff_Occurrence;

   type Diff_Occurrence_Link is access Diff_Occurrence;
   --  Linked list of diff occurrences

   type Diff_Occurrence is record
      Range1 : Diff_Range;
      Range2 : Diff_Range;
      Action : Diff_Action;
      Next   : Diff_Occurrence_Link;
   end record;

   function Diff (File1, File2 : String) return Diff_Occurrence_Link;
   --  Execute diff on File1 and File2 and return a list of differences.

   procedure Free (Link : in out Diff_Occurrence_Link);
   --  Free the memory associated with each node of the list Link.

   function Fine_Diff (Line1, Line2 : String) return Diff_Occurrence_Link;
   --  Do a fine diff between two lines.
   --  The only fields set in the resulting list is Range1 and Next, other
   --  fields should be ignored.

end Diff_Utils;
