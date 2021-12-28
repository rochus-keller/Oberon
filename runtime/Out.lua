local tochar = require("string").char
local ffi = require("ffi")
local module = {}

-- just a quick first implementation

function module.Open()
end

function module.Char(ch)
    if type(ch) == "number" then
        io.stdout:write(tochar(ch))
    else
        io.stdout:write(tostring(ch))
    end
end

function module.String(s)
	if type(s) == "cdata" then
		io.stdout:write(ffi.string(s))
	else
	    io.stdout:write(tostring(s))
	end
end

function module.Int(i,n)
        io.stdout:write(string.format("%"..tostring(n).."d",i))
        -- io.stdout:write(string.format("%d",i) )
end

function module.Real(x,n)
    -- io.stdout:write(x) -- effect of n not properly specified
    io.stdout:write(string.format("%"..tostring(n).."e",x))
end

function module.Ln()
    io.stdout:write("\n")
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Out.Def changes!
module[0] = module.Open
module[1] = module.Char
module[2] = module.String
module[3] = module.Int
module[4] = module.Real
module[5] = module.Ln

return module
