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
	typedef struct{
    	uint32_t keys;
    	int x, y;
	} InputState;

	void ObsDisplay_getState( InputState* s );
]]

local state = ffi.new("InputState");

function module.RegisterMouseHandler(func)
	Display_RegisterHandler(func,1)
end

function module.RegisterCharHandler(func)
	Display_RegisterHandler(func,2)
end

function module.RegisterIdleHandler(func)
	Display_RegisterHandler(func,3)
end

function module.Available() -- (): INTEGER
	return 0 -- obsolete
end

function module.Read() --(VAR ch: CHAR)
	return nil, 0 -- obsolete
end

function module.Mouse() -- (VAR keys: SET; VAR x, y: INTEGER)
  	C.ObsDisplay_getState(state)
	return nil, state.keys, state.x, state.y
end

function module.SetMouseLimits(w,h)
	-- obsolete
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Input.Def changes!
module[0] = module.RegisterMouseHandler -- procedure
module[1] = module.RegisterCharHandler -- procedure
module[2] = module.RegisterIdleHandler -- procedure
module[3] = module.Available -- procedure
module[4] = module.Read -- procedure
module[5] = module.Mouse -- procedure
module[6] = module.SetMouseLimits -- procedure

Input = module -- make it globally visible

return module
