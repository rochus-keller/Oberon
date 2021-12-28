local module = {}
local clock = os.clock

function module.Time()
    return clock() * 1000.0 * 1000.0
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Input.Def changes!
--module[0] = module.Available
--module[1] = module.Read
--module[2] = module.Mouse
--module[3] = module.SetMouseLimits
module[4] = module.Time

return module
