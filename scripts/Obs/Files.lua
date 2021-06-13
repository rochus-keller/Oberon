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

ffi.cdef[[
	typedef struct {
		void* d_buf;
	} FileBuffer;
	typedef uint8_t CharArray[?];
	
	int ObsFiles_openFile( CharArray filename, FileBuffer* fb );
	void ObsFiles_freeFile( FileBuffer* fb );
	int ObsFiles_newFile( FileBuffer* fb );
	int ObsFiles_saveFile( CharArray filename, FileBuffer* fb );
	int ObsFiles_removeFile( CharArray filename );
	int ObsFiles_renameFile( CharArray oldName, CharArray newName );
	uint32_t ObsFiles_length( FileBuffer* fb );
	int ObsFiles_setPos( FileBuffer* fb, uint32_t pos );
	uint32_t ObsFiles_getPos( FileBuffer* fb );
	int ObsFiles_atEnd( FileBuffer* fb );
	int ObsFiles_writeByte( FileBuffer* fb, uint32_t byte );
	uint32_t ObsFiles_readByte( FileBuffer* fb );
	int ObxFfi_DIV( int a, int b );
	int ObxFfi_MOD( int a, int b );
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
local FileBuffer = ffi.typeof("FileBuffer")
local errmsg = "invalid FileDesc instance"

function module.Old(name) -- (name: ARRAY OF CHAR): File;
	local fb = ffi.new(FileBuffer)
	fb.d_buf = nil
	ffi.gc(fb, C.ObsFiles_freeFile)
	C.ObsFiles_openFile(name, fb)
	local f = {}
	f[0] = name
	f[1] = fb
	setmetatable(f,FileDesc)
	return f
end

function module.New(name) -- (name: ARRAY OF CHAR): File
	local fb = ffi.new(FileBuffer)
	fb.d_buf = nil
	ffi.gc(fb, C.ObsFiles_freeFile)
	C.ObsFiles_newFile(fb)
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
	return C.ObsFiles_removeFile(name) ~= 0
end

function module.Rename(old, _new) -- (old, new: ARRAY OF CHAR; VAR res: INTEGER)
	return C.ObsFiles_renameFile(old,_new) ~= 0
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
end

function module.Pos(r) -- (VAR r: Rider): INTEGER
	if r[3] and r[3][1] then
		return C.ObsFiles_getPos(r[3][1])
	else
		return 0
	end
end

function module.Base(r) -- (VAR r: Rider): File
	return r[3]
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
	return ReadByte(r)
end

function module.Read(r) -- (VAR r: Rider; VAR ch: CHAR)
	return ReadByte(r)
end

function module.ReadInt(r) -- (VAR R: Rider; VAR x: INTEGER)
	local x0 = ReadByte(r)
	local x1 = ReadByte(r)
	local x2 = ReadByte(r)
	local x3 = ReadByte(r)
	return ((x3 * 0x100 + x2) * 0x100 + x1) * 0x100 + x0
end

function module.ReadString(r, x) -- (VAR R: Rider; VAR x: ARRAY OF CHAR)
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
	return x
end

function module.ReadNum(r) -- (VAR R: Rider; VAR x: INTEGER)
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
    return x
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
end

function module.Write(r,ch) -- (VAR r: Rider; ch: CHAR)
	r[1] = 0
	WriteByte(r,x)
end

function module.WriteInt(r,x) -- (VAR R: Rider; x: INTEGER)
    r[1] = 0
    WriteByte(r,C.ObxFfi_MOD(x,0x100))
    WriteByte(r,C.ObxFfi_MOD(C.ObxFfi_DIV(x,0x100),0x100))
    WriteByte(r,C.ObxFfi_MOD(C.ObxFfi_DIV(x,0x10000),0x100))
    WriteByte(r,C.ObxFfi_MOD(C.ObxFfi_DIV(x,0x1000000),0x100))
end

function module.WriteString(r,x) -- (VAR R: Rider; x: ARRAY OF CHAR)
    r[1] = 0
    local i = 0
    local ch
    repeat
        ch = x[i]
        WriteByte(r,ch)
        i = i + 1
    until( ch == 0x0 )
end

function module.WriteNum(r,x) -- (VAR R: Rider; x: INTEGER)
    r[1] = 0
    while x < -0x40 or x >= 0x40 do
        WriteByte( r, C.ObxFfi_MOD( x, 0x80 + 0x80) )
        x = basr(x, 7)
    end
    WriteByte( C.ObxFfi_MOD( x, 0x80 ) )
end

function module.RestoreList()
	-- NOP
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if Files.Def changes!
module[0] = module.Old
module[1] = module.New
module[2] = module.Register
module[3] = module.Delete
module[4] = module.Rename
module[5] = module.Length
module[6] = module.Set
module[7] = module.Pos
module[8] = module.Base
module[9] = module.ReadByte
module[10] = module.Read
module[11] = module.ReadInt
module[12] = module.ReadString
module[13] = module.ReadNum
module[14] = module.WriteByte
module[15] = module.Write
module[16] = module.WriteInt
module[17] = module.WriteString
module[18] = module.WriteNum
module[19] = module.RestoreList
module[20] = Rider
module[21] = FileDesc

return module
