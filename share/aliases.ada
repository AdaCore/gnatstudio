<?xml version="1.0"?>
<Aliases>
  <alias name="procedure is" >
    <param name="name"  />
    <param name="params"  />
    <text>procedure %(name) (%(params)) is
begin
   %_
end %(name);</text>
  </alias>
  <alias name="task body" >
    <param name="name"  />
    <text>task body %(name) is
begin
   %_
end %(name);</text>
  </alias>
  <alias name="select" >
    <param name="entry2"  />
    <param name="entry"  />
    <text>select
   accept %(entry) do
      %_
   end %(entry);
or
   accept %(entry2) do
      null;
   end %(entry2);
end select;</text>
  </alias>
  <alias name="function is" >
    <param name="name"  />
    <param name="params"  />
    <param name="return_type"  />
    <text>function %(name) (%(params)) return %(return_type) is
begin
   return %_
end %(name);</text>
  </alias>
  <alias name="declare" >
    <text>declare
   %_
begin
   null;
end;</text>
  </alias>
  <alias name="for" >
    <param name="range"  />
    <param name="index"  />
    <text>for %(index) in %(range) loop
   %_
end loop;</text>
  </alias>
  <alias name="procedure" >
    <param name="name"  />
    <text>procedure %(name)%_;</text>
  </alias>
  <alias name="package body" >
    <param name="name"  />
    <text>package body %(name) is

   %_

end %(name);</text>
  </alias>
  <alias name="task" >
    <param name="name"  />
    <text>task %(name) is
   %_
end %(name);</text>
  </alias>
  <alias name="loop" >
    <param name="exit_condition"  />
    <text>loop
   %_
   exit when %(exit_condition);
end loop;</text>
  </alias>
  <alias name="case" >
    <param name="choice"  />
    <param name="expression"  />
    <text>case %(expression) is
   when %(choice) =&gt;
      %_
end case;</text>
  </alias>
  <alias name="while" >
    <param name="condition"  />
    <text>while %(condition) loop
   %_
end loop;</text>
  </alias>
  <alias name="package" >
    <param name="name"  />
    <text>package %(name) is

   %_

end %(name);</text>
  </alias>
  <alias name="if" >
    <param name="condition"  />
    <text>if %(condition) then
   %_
end if;</text>
  </alias>
  <alias name="function" >
    <param name="name"  />
    <param name="params"  />
    <text>function %(name) (%(params)) return %_;</text>
  </alias>
  <alias name="array" >
    <param name="range"  />
    <text>array (%(range)) of %_;</text>
  </alias>
  <alias name="exception" >
    <param name="error"  />
    <text>exception
   when %(error) =&gt;
      %_</text>
  </alias>
  <alias name="begin" >
    <text>begin
   %_
end;</text>
  </alias>
</Aliases>
