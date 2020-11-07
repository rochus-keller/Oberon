(* Example from https://en.wikipedia.org/wiki/Oberon-2,
    v3 + generic type and left out unused ListNode type *)
    
module Lists

    // declare global constants, types and variables:
    
    type
        List*<T>    = ^record
            value : T
            next  : List<T>
        end

    // declare procedures:
    
    proc<T> (l : List<T>) Add* (v : T)
    do
        if l = nil then
            new(l)              // create record instance
            l.value := v
        else
            l.next.Add(v)      // recursive call to .add(n)
        end
    end Add

    proc<T> (l : List<T>) Get* () : T
    var
        v : T  // v is automatically initialized to the default value of T
    do
        if l # nil then
            v := l.value      //  this line will crash if l is nil 
            l := l.next
        end
        return v
    end Get

end Lists
