pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   --  unsupported macro: NULL __null
   subtype size_t is unsigned_long;  -- /export/work/setton/src/GPS/sandbox/x86_64-linux/stable-gnat/install/bin/../lib/gcc/x86_64-pc-linux-gnu/4.7.4/include/stddef.h:213

end stddef_h;
