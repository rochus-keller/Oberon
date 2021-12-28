--[[
* Copyright 2019, 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
]]--

local _lib = require 'obnljlib'
local ffi = require 'ffi'
local C = ffi.C

local module = {}
module.C = ffi.C

function module.instance( class )
	local obj = {}
        if class ~= nil then
            setmetatable(obj,class)
	end
	return obj
end

function module.is_a( obj, class )
	local meta = getmetatable(obj)
	while meta and class and meta ~= class do
		meta = getmetatable(meta)
	end
	return meta == class
end

local function runThunk( table, set, val )
	if set then
		table.table[table.index] = val
	else
		return table.table[table.index]
	end
end

function module.thunk( table, index )
	t = { table = table, index = index, __call = runThunk }
	setmetatable( t, t )
	return t
end

function module.PACK( x, n )
	x( true, _lib.PACK( x(), n ) )
end

function module.PACK_NT( x, n )
        return _lib.PACK( x, n )
end

function module.UNPK( x, n )
	local _x, _n = _lib.UNPK( x(), n() )
	x( true, _x )
	n( true, _n )
end

function module.UNPK_NT( x, n )
        return _lib.UNPK( x, n );
end

function module.Arr(n)
    local t = {}
    t.n = n
    return t
end

ffi.cdef[[
    int LjLibFfi_DIV( int a, int b );
    int LjLibFfi_MOD( int a, int b );
]]

module.DIV = C.LjLibFfi_DIV
module.MOD = C.LjLibFfi_MOD

function module.BOUNDS( index, table, file, line )
    -- _lib.TRACE( "bounds at "..file.." "..tonumber(line) )
    local len = table.n
    if len == nil then
        return -- only interested in Oberon arrays
    end
    if index > len or index < 1 then
        error( "array bounds violation at "..tostring(file).." line "..tostring(line) )
    end
end

module.SET = _lib.SET
module.IN = _lib.IN
module.ORD = _lib.ORD
module.ASSERT = _lib.ASSERT
module.TRACE = _lib.TRACE
module.INCL = _lib.INCL
module.EXCL = _lib.EXCL
module.Str = _lib.Str
module.Char = _lib.Char
module.TRAP = _lib.TRAP
module.Copy = _lib.Copy

return module
