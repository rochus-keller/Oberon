--[[
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
]]--

local module = {}

local ffi = require 'ffi'
local C = ffi.C

ffi.cdef[[
	uint32_t ObsDisplay_getTime();
        int ObsDisplay_getClock();
        void ObsDisplay_setClock(int dt);
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
module[0] = module.Time -- procedure
module[1] = module.Clock -- procedure
module[2] = module.SetClock -- procedure
module[3] = 0 -- allocated -- variable
module[4] = 0 -- NofSectors -- variable
module[5] = 0 -- heapOrg -- variable
module[6] = 0 -- heapLim -- variable
module[7] = 0 -- stackOrg -- variable


Kernel = module -- make it globally visible

return module
