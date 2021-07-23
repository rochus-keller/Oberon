(* Example from https://en.wikipedia.org/wiki/Oberon-2,
    v3 + generics and left out unused ListNode type *)
    
module Lists(T)

    // declare global constants, types and variables:
    
    type
        List*    = ^record
            value : T
            next  : List
        end

    // declare procedures:
    
    proc (l : List) Add* (v : T)
    begin
        if l = nil then
            new(l)              // create record instance
            l.value := v
        else
            l.next.Add(v)      // recursive call to .add(n)
        end
    end Add

    proc (l : List) Get* () : T
    var
        v : T 
    begin
        if l # nil then
            v := l.value      //  this line will crash if l is nil 
            l := l.next
        end
        return v
    end Get

end Lists
