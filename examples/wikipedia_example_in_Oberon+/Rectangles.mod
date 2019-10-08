module Rectangles;

import Figures;

type
   Rectangle* = pointer to RectangleDesc;

   RectangleDesc* = record
      (Figures.FigureDesc)
      x, y, w, h : integer;
   end;

var
   if_ : Figures.Interface;

procedure New* (var r : Rectangle);
begin
   new(r);
   Figures.Init(r, if_);
end New;

procedure Draw* (f : Figures.Figure);
   var
      r : Rectangle;
begin
   r := f(Rectangle); (* f AS Rectangle *)
   (* ... *)
end Draw;

(* Other procedures here *)

begin (* Module initialisation *)
   New(if_);
   if_.draw  := Draw;
   (*if_.clear := Clear;
   if_.mark  := Mark;
   if_.move  := Move;*)
end Rectangles.
