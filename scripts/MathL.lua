local module = {}
local math = require("math")

-- TODO

function module.round(x)
	return math.floor(x+0.5)
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Math.Def changes!
module[0] = math.sqrt
module[1] = math.pow
module[2] = math.exp
module[3] = math.log
module[4] = module.log -- TODO
module[5] = module.round
module[6] = math.sin
module[7] = math.cos
module[8] = math.tan
module[9] = math.asin
module[10] = math.acos
module[11] = math.atan
module[12] = math.atan2
module[13] = math.sinh
module[14] = math.cosh
module[15] = math.tanh
module[16] = module.arcsinh -- TODO
module[17] = module.arccosh -- TODO
module[18] = module.arctanh -- TODO

return module
