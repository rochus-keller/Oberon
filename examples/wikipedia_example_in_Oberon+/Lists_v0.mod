(* example from https://en.wikipedia.org/wiki/Oberon-2,
	modified Get for Oberon-07 compatibility *)

MODULE Lists;

    (*** declare global constants, types and variables ***)

    TYPE
        List*    = POINTER TO ListNode;
        ListNode = RECORD
            value : Integer;
            next  : List;
        END;

    (*** declare procedures ***)

    PROCEDURE (l : List) Add* (v : Integer);
    BEGIN
        IF l = NIL THEN
            NEW(l);             (* create record instance *)
            l.value := v
        ELSE
            l.next.Add(v)      (* recursive call to .add(n) *)
        END
    END Add;

    PROCEDURE (l : List) Get* () : Integer;
    VAR
        v : Integer;
    BEGIN
        IF l # NIL THEN
            v := l.value;       (* this line will crash if l is NIL *)
            l := l.next;
        END;
        RETURN v
    END Get;

END Lists.

