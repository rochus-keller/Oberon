local module = {}

function module.Time()
    return os.clock() * 1000
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Input.Def changes!
--module[0] = module.Available
--module[1] = module.Read
--module[2] = module.Mouse
--module[3] = module.SetMouseLimits
module[4] = module.Time

return module
