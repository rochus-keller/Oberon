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
	typedef uint8_t ByteArray[];
	void ObxFfi_initByteArrayFromHex( ByteArray ba, int count, const char* hex );
	void ObsDisplay_ReplConst(int color, int x, int y, int w, int h, int mode );
    void ObsDisplay_CopyPattern(int color, ByteArray patadr, int count, int x, int y, int mode );
    void ObsDisplay_CopyBlock(int sx, int sy, int w, int h, int dx, int dy, int mode);
    void ObsDisplay_Dot(int color, int x, int y, int mode);
    int ObxFfi_DIV( int a, int b );
]]

-- NOTE: sync these constants with definition file
local Width = 1024
local Height = 768
local patternLen = 32 

local arrow 
local star
local hook
local updown
local block

local function presetIcons()
	arrow = obxlj.createByteArray(patternLen)
	C.ObxFfi_initByteArrayFromHex(arrow,patternLen,"0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00")

	star = obxlj.createByteArray(patternLen)
	C.ObxFfi_initByteArrayFromHex(star,patternLen,"0F0F 8000 8220 8410 8808 9004 A002 C001 7F7F C001 A002 9004 8808 8410 8220 8000")

	hook = obxlj.createByteArray(patternLen)
	C.ObxFfi_initByteArrayFromHex(hook,patternLen,"0C0C 070F 8707 C703 E701 F700 7F00 3F00 1F00 0F00 0700 0300 01")

	updown = obxlj.createByteArray(patternLen)
	C.ObxFfi_initByteArrayFromHex(updown,patternLen,"080E 183C 7EFF 1818 1818 1818 FF7E3C18")

	block = obxlj.createByteArray(patternLen)
	C.ObxFfi_initByteArrayFromHex(block,patternLen,"0808 FFFF C3C3 C3C3 FFFF")
end

presetIcons()

function module.ReplConst(color, x, y, w, h, mode) -- (color, x, y, w, h, mode: INTEGER)
	C.ObsDisplay_ReplConst(color, x, y, w, h, mode)
end

function module.CopyPattern(color, patadr, x, y, mode ) -- (color: INTEGER; VAR patadr: ARRAY OF BYTE; x, y, mode: INTEGER)
        C.ObsDisplay_CopyPattern(color, patadr, ffi.sizeof(patadr), x, y, mode )
        return nil, patadr
end

function module.CopyBlock(sx, sy, w, h, dx, dy, mode) -- (sx, sy, w, h, dx, dy, mode: INTEGER)
        C.ObsDisplay_CopyBlock(sx, sy, w, h, dx, dy, mode)
end

function module.Dot(col, x, y, mode) -- (col, x, y, mode: INTEGER)
        C.ObsDisplay_Dot(col, x, y, mode)
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Display.Def changes!
module[0] = module.ReplConst -- procedure
module[1] = module.CopyPattern -- procedure
module[2] = module.CopyBlock -- procedure
module[3] = module.Dot -- procedure
module[4] = Width -- variable
module[5] = Height -- variable
module[6] = arrow -- variable
module[7] = star -- variable
module[8] = hook -- variable
module[9] = updown -- variable
module[10] = block -- variable
module[11] = {} -- FrameMsg -- record
module[12] = {} -- FrameDesc -- record

Display = module -- make it globally visible

return module
