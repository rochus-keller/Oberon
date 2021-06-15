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

local ffi = require 'ffi'
local C = ffi.C

ffi.cdef[[
	uint32_t ObsDisplay_getTime();
	uint32_t ObsDisplay_getClock();
	void ObsDisplay_setClock(uint32_t dt);
]]

function module.Time()
	return C.ObsDisplay_getTime()
end

function module.Clock()
	return C.ObsDisplay_getClock()
end

function module.SetClock(dt)
	C.ObsDisplay_setClock(dt)
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Kernel.Def changes!
module[0] = 0 -- allocated
module[1] = 0 -- NofSectors
module[2] = 0 -- heapOrg
module[3] = 0 -- heapLim
module[4] = 0 -- stackOrg
module[5] = module.Time
module[6] = module.Clock
module[7] = module.SetClock

Kernel = module -- make it globally visible

return module
