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

local obxlj = require 'obxlj'
local ffi = require 'ffi'
local bit = require 'bit'
local C = ffi.C
local bytesize = ffi.sizeof
local bror = bit.ror
local basr = bit.arshift
local isFfiString = obxlj.isFfiString

ffi.cdef[[
	typedef uint8_t CharArray[?];
	
	int ObsFiles_openFile( CharArray filename );
	void ObsFiles_freeFile( int fb );
	int ObsFiles_newFile();
	int ObsFiles_saveFile( CharArray filename, int fb );
	int ObsFiles_removeFile( CharArray filename );
	int ObsFiles_renameFile( CharArray oldName, CharArray newName );
	uint32_t ObsFiles_length(int fb );
        int ObsFiles_setPos( int fb, int pos );
        int ObsFiles_getPos( int fb );
	int ObsFiles_atEnd( int fb );
        int ObsFiles_writeByte( int fb, uint32_t byte );
	uint32_t ObsFiles_readByte( int fb );
]]

local Rider = {}
	-- Rider[0] eof: BOOLEAN
	-- Rider[1] res: INTEGER
	-- Rider[2] pos: INTEGER
	-- Rider[3] FileDesc or nil
local FileDesc = {}
	-- FileDesc[0] name or nil
	-- FileDesc[1] FileBuffer or nil

local CharArray = ffi.typeof("CharArray")
local errmsg = "invalid FileDesc instance"

function module.Old(name) -- (name: ARRAY OF CHAR): File;
	local fb = C.ObsFiles_openFile(name)
    if fb < 0 then
    	return nil
    end
	local f = {}
	f[0] = name
	f[1] = fb
	setmetatable(f,FileDesc)
	return f
end

function module.New(name) -- (name: ARRAY OF CHAR): File
	local fb = C.ObsFiles_newFile()
	local f = {}
	f[0] = name
	f[1] = fb
	setmetatable(f,FileDesc)
	return f
end

function module.Register(f) -- (f: File)
	if f[0] == nil or f[1] == nil then
		error(errmsg)
	end
	C.ObsFiles_saveFile(f[0], f[1])
end

function module.Delete(name) -- (name: ARRAY OF CHAR; VAR res: INTEGER)
	return nil, C.ObsFiles_removeFile(name) ~= 0
end

function module.Rename(old, _new) -- (old, new: ARRAY OF CHAR; VAR res: INTEGER)
	return nil, C.ObsFiles_renameFile(old,_new) ~= 0
end

function module.Length(f) -- (f: File): INTEGER
	if f[1] == nil then
		error(errmsg)
	end
	return C.ObsFiles_length(f[1])
end

function module.Set(r,f,pos) -- (VAR r: Rider; f: File; pos: INTEGER)
	r[0] = false -- eof
	r[1] = 0 -- res
	r[2] = pos
	r[3] = f
	return nil, r
end

function module.Pos(r) -- (VAR r: Rider): INTEGER
	return r[2], r
end

function module.Base(r) -- (VAR r: Rider): File
	return r[3], r
end

local function ReadByte(r) 
	if r[3] == nil or r[3][1] == nil then
		error(errmsg)
	end
        C.ObsFiles_setPos(r[3][1], r[2])
	r[0] = false
	r[1] = 0
	if C.ObsFiles_atEnd(r[3][1]) ~= 0 then
		r[0] = true
		r[1] = 1
		return 0
	end
	local byte = C.ObsFiles_readByte(r[3][1])
	local pos = C.ObsFiles_getPos(r[3][1])
	if pos == r[2] then
		r[1] = 1
	else
		r[2] = pos
	end
	return byte	
end

function module.ReadByte(r) -- (VAR r: Rider; VAR x: BYTE)
	return nil, r, ReadByte(r)
end

function module.Read(r) -- (VAR r: Rider; VAR ch: CHAR)
	return nil, r, ReadByte(r)
end

function module.ReadInt(r) -- (VAR r: Rider; VAR x: INTEGER)
	local x0 = ReadByte(r)
	local x1 = ReadByte(r)
	local x2 = ReadByte(r)
	local x3 = ReadByte(r)
	return nil, r, ((x3 * 0x100 + x2) * 0x100 + x1) * 0x100 + x0
end

function module.ReadString(r, x) -- (VAR r: Rider; VAR x: ARRAY OF CHAR)
	local len = bytesize(x)
	local i = 0
    local ch = ReadByte(r)
    while ch ~= 0 do
      if i < len-1 then
      	x[i] = ch
      	i = i + 1
      end
      ch = ReadByte(r)
    end
	return nil, r, x
end

function module.ReadNum(r) -- (VAR r: Rider; VAR x: INTEGER)
    local n = 32
    local y = 0
    local b = ReadByte(r)
    while b >= 0x80 do
        y = bror( y + b-0x80, 7)
        n = n - 7
        b = ReadByte(r)
    end
    local x
    if n <= 4 then
        x = bror( C.ObxFfi_MOD(y + b, 0x10), 4)
    else
        x = basr( ROR(y + b, 7), n-7 )
    end
    return nil, r, x
end

local function WriteByte(r,x) 
	if r[3] == nil or r[3][1] == nil then
		error(errmsg)
        end
	C.ObsFiles_setPos(r[3][1], r[2])
	C.ObsFiles_writeByte(r[3][1],x)
	local pos = C.ObsFiles_getPos(r[3][1])
	if pos == r[2] then
		r[1] = r[1] + 1
	else
		r[2] = pos
	end
end

function module.WriteByte(r,x) -- (VAR r: Rider; x: BYTE)
	r[1] = 0
	WriteByte(r,x)
	return nil, r
end

function module.Write(r,ch) -- (VAR r: Rider; ch: CHAR)
	r[1] = 0
	if isFfiString(ch) then
		WriteByte(r,ch[0]) 
	else
		WriteByte(r,ch)
	end
	return nil, r
end

function module.WriteInt(r,x) -- (VAR r: Rider; x: INTEGER)
    r[1] = 0
    WriteByte(r,C.ObxFfi_MOD(x,0x100))
    WriteByte(r,C.ObxFfi_MOD(C.ObxFfi_DIV(x,0x100),0x100))
    WriteByte(r,C.ObxFfi_MOD(C.ObxFfi_DIV(x,0x10000),0x100))
    WriteByte(r,C.ObxFfi_MOD(C.ObxFfi_DIV(x,0x1000000),0x100))
    return nil, r
end

function module.WriteString(r,x) -- (VAR r: Rider; x: ARRAY OF CHAR)
    r[1] = 0
    local i = 0
    local ch
    repeat
        ch = x[i]
        WriteByte(r,ch)
        i = i + 1
    until( ch == 0x0 )
    return nil, r
end

function module.WriteNum(r,x) -- (VAR r: Rider; x: INTEGER)
    r[1] = 0
    while x < -0x40 or x >= 0x40 do
        WriteByte( r, C.ObxFfi_MOD( x, 0x80 + 0x80) )
        x = basr(x, 7)
    end
    WriteByte( C.ObxFfi_MOD( x, 0x80 ) )
    return nil, r
end

function module.RestoreList()
	-- NOP
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Files.Def changes!
module[0] = module.Old -- procedure
module[1] = module.New -- procedure
module[2] = module.Register -- procedure
module[3] = module.Delete -- procedure
module[4] = module.Rename -- procedure
module[5] = module.Length -- procedure
module[6] = module.Set -- procedure
module[7] = module.Pos -- procedure
module[8] = module.Base -- procedure
module[9] = module.ReadByte -- procedure
module[10] = module.Read -- procedure
module[11] = module.ReadInt -- procedure
module[12] = module.ReadString -- procedure
module[13] = module.ReadNum -- procedure
module[14] = module.WriteByte -- procedure
module[15] = module.Write -- procedure
module[16] = module.WriteInt -- procedure
module[17] = module.WriteString -- procedure
module[18] = module.WriteNum -- procedure
module[19] = module.RestoreList -- procedure
module[20] = Rider -- record
module[21] = FileDesc -- record

Files = module -- make it globally visible

return module
