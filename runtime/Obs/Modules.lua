--[[
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
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

local module = {}

local obxlj = require("obxlj")
local ffi = require 'ffi'
local C = ffi.C

ffi.cdef[[
	int ObxFfi_strRelOp( char* lhs, char* rhs, int op );
]]

local ModDesc = {}
local root = 3
local res = 5
local importing = 6
local imported = 7

-- module[3] = root -- variable
-- module[4] = AllocPtr -- variable
-- module[5] = res -- variable
-- module[6] = importing -- variable
-- module[7] = imported -- variable

module[res] = 0 -- res
module[importing] = obxlj.charToStringArray(1,"") -- importing
module[imported] = obxlj.charToStringArray(1,"") -- imported

local function findModule(name)
	local cur = module[root] -- root
	while cur do
		if C.ObxFfi_strRelOp(cur[0],name,0) ~= 0 then -- cur[0] is name
			return cur
		else
			cur = cur[1] -- cur[1] is next
		end
	end
	return nil
end

function module.Load(name) -- (name: ARRAY OF CHAR; VAR newmod: Module)
	module[importing] = name -- importing
	local m = findModule(name)
	if m == nil then
		-- we don't really load a module here because all modules are already loaded
		m = {}
		setmetatable(m,ModDesc)
		m[0] = name
		m[1] = module[root] -- next = root
		for i=2,11 do
			m[i] = 0
		end
		module[root] = m
	end
	module[res] = 0 -- res
	module[imported] = name -- imported
	return nil, m
end

function module.ThisCommand(mod, name) -- (mod: Module; name: ARRAY OF CHAR): Command
	module[res] = 5 -- res
	if mod == nil then
		return nil
	end
	local modName = ffi.string(mod[0]) -- name
	local funcName = ffi.string(name)
	local modTbl = _G[modName]
	local func
	if modTbl then
		func = modTbl[ funcName ]
	end
	if func then
		module[res] = 0 -- res
	end
	return func
end

function module.Free(name)
	-- NOP
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Modules.Def changes!
module[0] = module.Load -- procedure
module[1] = module.ThisCommand -- procedure
module[2] = module.Free -- procedure
module[8] = ModDesc -- record

Modules = module -- make it globally visible

return module
