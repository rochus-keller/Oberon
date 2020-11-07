(* example from https://en.wikipedia.org/wiki/Oberon-2,
	v1 + short keywords + line comments *)

module Lists;

    // declare global constants, types and variables

    type
        List*    = ^ListNode;
        ListNode = record
            value : integer;
            next  : List;
        end;

    // declare procedures 

    proc (l : List) Add* (v : integer);
    do
        if l = NIL then
            new(l);             // create record instance
            l.value := v
        else
            l.next.Add(v)      // recursive call to .add(n)
        end
    end Add;

    proc (l : List) Get* () : integer;
    var
        v : integer;
    do
        if l # NIL then
            v := l.value;       // this line will crash if l is NIL
            l := l.next;
        end;
        return v
    end Get;

end Lists.

