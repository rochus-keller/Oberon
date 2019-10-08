(* Example from https://en.wikipedia.org/wiki/Oberon_(programming_language) 
    changed to Oberon+ syntax *)

module Figures; // Abstract module

type
   Figure*    = pointer to FigureDesc;
   Interface* = pointer to InterfaceDesc;

   InterfaceDesc* = record
      draw*  : procedure (f : Figure);
      clear* : procedure (f : Figure);
      mark*  : procedure (f : Figure);
      move*  : procedure (f : Figure; dx, dy : integer);
   end;

   FigureDesc* = record
      if_: Interface;
   end;

procedure Init* (f : Figure; if_ : Interface);
begin
   f.if_ := if_;
end Init;

procedure Draw* (f : Figure);
begin
   f.if_.draw(f);
end Draw;

// Other procedures here 

end Figures.
