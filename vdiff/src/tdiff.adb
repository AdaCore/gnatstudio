with Diff_Utils; use Diff_Utils;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure TDiff is

   procedure Print (R : Diff_Range);
   --  Print a given line range on standard output.

   procedure Print (R : Diff_Range) is
   begin
      if R.First = R.Last then
         Put (Positive'Image (R.First));
      else
         Put (Positive'Image (R.First));
         Put (" ->");
         Put (Positive'Image (R.Last));
      end if;
   end Print;

   Result, Tmp : Diff_Occurrence_Link;

begin
   if Argument_Count /= 2 then
      Put_Line ("incorrect number of parameters. exiting.");
      return;
   end if;

   Result := Diff_Utils.Diff (Argument (1), Argument (2));
   Tmp := Result;

   loop
      exit when Tmp = null;

      Put (Diff_Action'Image (Tmp.Action));
      Put (" from");
      Print (Tmp.Range1);
      Put (" to");
      Print (Tmp.Range2);
      New_Line;

      Tmp := Tmp.Next;
   end loop;

   Free (Result);
end TDiff;
