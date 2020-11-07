(* example from https://en.wikipedia.org/wiki/Oberon-2,
	v0 + lower-caps keywords *)

module Lists;

    (*** declare global constants, types and variables ***)

    type
        List*    = pointer to ListNode;
        ListNode = record
            value : integer;
            next  : List;
        end;

    (*** declare procedures ***)

    procedure (l : List) Add* (v : integer);
    begin
        if l = NIL then
            new(l);             (* create record instance *)
            l.value := v
        else
            l.next.Add(v)      (* recursive call to .add(n) *)
        end
    end Add;

    procedure (l : List) Get* () : integer;
    var
        v : integer;
    begin
        if l # NIL then
            v := l.value;       (* this line will crash if l is NIL *)
            l := l.next;
        end;
        return v
    end Get;

end Lists.

