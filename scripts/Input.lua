local module = {}

function module.Time()
    return os.clock() * 1000
end

return module
