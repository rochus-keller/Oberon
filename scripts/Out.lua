local module = {}

-- just a quick first implementation

function module.Open()
end

function module.Char(ch)
	print(ch)
end

function module.String(s)
	print(s)
end

function module.Int(i,n)
	print(i)
end

function module.Hex(i)
	print(i)
end

function module.Real(x,n)
	print(x)
end

function module.Ln()
	print("\n")
end

return module
