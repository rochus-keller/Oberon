local module = {}
local math = require("math")

-- TODO

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Math.Def changes!
module[0] = module.sqrt
module[1] = module.power
module[2] = module.exp
module[3] = module.ln
module[4] = module.log
module[5] = module.round
module[6] = module.sin
module[7] = module.cos
module[8] = module.tan
module[9] = module.arcsin
module[10] = module.arccos
module[11] = module.arctan
module[12] = module.arctan2
module[13] = module.sinh
module[14] = module.cosh
module[15] = module.tanh
module[16] = module.arcsinh
module[17] = module.arccosh
module[18] = module.arctanh

return module
