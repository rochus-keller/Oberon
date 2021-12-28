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

local ffi = require 'ffi'
local C = ffi.C
local string = require 'string'
local io = require 'io'
local math = require 'math'
local bit = require 'bit'
local os = require 'os'
local jit = require 'jit'

local module = {}
obxlj = module -- directly publish the module by global var

ffi.cdef[[
    int ObxFfi_DIV( int a, int b );
    int ObxFfi_MOD( int a, int b );
    
	typedef int16_t ShortArray[?];
	typedef int32_t IntArray[?];
	typedef int64_t LongArray[?];
	typedef float FloatArray[?];
	typedef double DoubleArray[?];
	typedef uint32_t UIntArray[?];
	typedef uint8_t CharArray[?];
	typedef uint16_t WcharArray[?];
	typedef uint8_t ByteArray[?];
	typedef bool BoolArray[?];
	
	void ObxFfi_initString( CharArray data, int count, const char* utf8 ); 
	void ObxFfi_initWstring( WcharArray wa, int count, const char* utf8 );
	void ObxFfi_initByteArray( ByteArray ba, int count, const char* data );
	int ObxFfi_strRelOp( char* lhs, char* rhs, int op ); 
	int ObxFfi_wstrRelOp( WcharArray lhs, int lcount, WcharArray rhs, int rcount, int op );
	void ObxFfi_printString( const char* str );
	void ObxFfi_printWcharArray( WcharArray wa, int count );
	void ObxFfi_DBGTRACE( const char* str );
	void ObxFfi_CRASH(int);
	void ObxFfi_TRACE( const char* str );
	void ObxFfi_NOP();
]]

local BoolArray = ffi.typeof("BoolArray")
local CharArray = ffi.typeof("CharArray")
local ByteArray = ffi.typeof("ByteArray")
local WcharArray = ffi.typeof("WcharArray")
local ShortArray = ffi.typeof("ShortArray")
local IntArray = ffi.typeof("IntArray")
local LongArray = ffi.typeof("LongArray")
local UIntArray = ffi.typeof("UIntArray")
local FloatArray = ffi.typeof("FloatArray")
local DoubleArray = ffi.typeof("DoubleArray")
local bytesize = ffi.sizeof
local frexp = math.frexp

function module.charToStringArray(len, str)
	local a = ffi.new( CharArray, len ) 
	if str then
		C.ObxFfi_initString(a, len, str)
	end
	return a
end
function module.createWcharArray(len, str)
	local a = ffi.new( WcharArray, len ) 
	if str then
		C.ObxFfi_initWstring(a, len, str)
	end 
	return a
end
function module.createByteArray(len, data)
	local a = ffi.new( ByteArray, len ) 
	if data then
		C.ObxFfi_initByteArray(a,len,data)
	end
	return a
end
function module.createBoolArray(len)
	return ffi.new( BoolArray, len ) 
end
function module.createShortArray(len)
	return ffi.new( ShortArray, len ) 
end
function module.createIntArray(len)
	return ffi.new( IntArray, len ) 
end
function module.createLongArray(len)
	return ffi.new( LongArray, len ) 
end
function module.createSetArray(len)
	return ffi.new( UIntArray, len )
end
function module.createFloatArray(len)
	return ffi.new( FloatArray, len )
end
function module.createDoubleArray(len)
	return ffi.new( DoubleArray, len )
end
function module.createLuaArray(len)
	local a = { count = len }
	return a
end
local function addElemToSet( set, elem )
	return bit.bor( set, bit.lshift( 1, elem ) )
end
function module.removeElemFromSet( set, elem )
	return bit.band( set, bit.bnot( bit.lshift( 1, elem ) ) )
end
function module.addRangeToSet( set, from, to )
	if from > to then
		return set
	end
	for i=from,to do
		set = addElemToSet(set,i)
	end
	return set
end
local function strlen( str, wide )
	local count = bytesize(str)
	if wide then
		count = count / 2
	end
	for i=0,count-1 do
		if str[i] == 0 then
			return i
		end
	end
	return count
end
function module.joinStrings( lhs, rhs, lwide, rwide )
	local lhslen = strlen(lhs,lwide)
	local rhslen = strlen(rhs,rwide)
	local count = lhslen + rhslen + 1
	local res
	if lwide or rwide then
		res = ffi.new( WcharArray, count )
	else
		res = ffi.new( CharArray, count )
	end
	local i
	for i = 0,lhslen-1 do
		res[i] = lhs[i]
	end
	for i = 0,rhslen-1 do
		res[i+lhslen] = rhs[i]
	end
	res[lhslen+rhslen] = 0
	return res
end
function module.charToString(ch,forceWide)
	local a 
	if ch > 255 or forceWide then
		a = ffi.new( WcharArray, 2 )
	else
		a = ffi.new( CharArray, 2 ) 
	end
	a[0] = ch
	a[1] = 0
	return a
end
local function toWide(str)
	local count = bytesize(str)
	local res = ffi.new( WcharArray, count )
	for i=0,count do
		res[i] = str[i]
	end
	return res
end
function module.stringRelOp( lhs, wideL, rhs, wideR, op )
	if wideL or wideR then
		if not wideL then
			lhs = toWide(lhs)
		end
		if not wideR then
			rhs = toWide(rhs)
		end
		return C.ObxFfi_wstrRelOp(lhs,bytesize(lhs)/2,rhs,bytesize(rhs)/2,op) ~= 0
	else
		return C.ObxFfi_strRelOp(lhs,rhs,op) ~= 0
	end	
end
function module.setSub( lhs, rhs )
	rhs = bit.bnot(rhs)
	return bit.band( lhs, rhs )
end
function module.setDiv( lhs, rhs )
	local tmp1 = bit.bnot( bit.band( lhs, rhs ) )
	local tmp2 = bit.bor( lhs, rhs )
	return bit.band( tmp1, tmp2 )
end
function module.setTest( elem, set )
	return bit.band( set, bit.lshift( 1, elem ) ) ~= 0
end
function module.is_a( obj, class )
	local meta = getmetatable(obj)
	while meta and class and meta ~= class do
		meta = getmetatable(meta)
	end
	return meta == class
end
local function printArray(arr)
	-- TODO
end
function module.println( val )
	if ffi.istype(CharArray,val) then
		C.ObxFfi_printString(val)
	elseif ffi.istype(WcharArray,val) then
		C.ObxFfi_printWcharArray(val,bytesize(val)/2)
	elseif ffi.istype(ByteArray,val) then
		printArray(val)
	elseif ffi.istype(ShortArray,val) then
		printArray(val)
	elseif ffi.istype(IntArray,val) then
		printArray(val)
	elseif ffi.istype(LongArray,val) then
		printArray(val)
	elseif ffi.istype(FloatArray,val) then
		printArray(val)
	elseif ffi.istype(DoubleArray,val) then
		printArray(val)
	else
		C.ObxFfi_printString(tostring(val))
	end
end
function module.isFfiString( val )
	return ffi.istype(CharArray,val) or ffi.istype(WcharArray,val)
end
function module.strcpy( lhs, rhs )
	local i = 0
	while rhs[i] ~= 0 do
		lhs[i] = rhs[i]
		i = i + 1
	end
	lhs[i] = 0
end
function module.ODD(num)
	return ( num % 2 ) == 1
end
function module.bool_to_number(value)
  return value and 1 or 0
end
function module.UNPACK(value)
	local x,n = frexp(value)
	x = x + x
	n = n - 1
	return x, n
end
function module.min_size(lhs,rhs)
	local l = bytesize(lhs)
	local r = bytesize(rhs)
	if l <= r then
		return l
	else
		return r
	end
end
local firstLock = true
function module.ldmod(name)
	if firstLock then
		firstLock = false
		return false
	end
	local m = require(ffi.string(name))
	-- print("LDMOD "..ffi.string(name).." "..tostring(m ~= nil)) 
	return m ~= nil
end
function module.ldcmd(mod,cmd)
	local m = require(ffi.string(mod))
	if m then return m[ffi.string(cmd)] end
	return nil
end

-- Magic mumbers used by the compiler
module[1] = module.charToStringArray
module[2] = module.createWcharArray
module[3] = module.createShortArray
module[4] = module.createIntArray
module[5] = module.createLongArray
module[6] = module.createFloatArray
module[7] = module.createDoubleArray
module[8] = module.createByteArray
module[9] = addElemToSet
module[10] = module.addRangeToSet
module[11] = bit.bnot
module[12] = bit.bor
module[13] = module.joinStrings
module[14] = C.ObxFfi_DIV
module[15] = C.ObxFfi_MOD
module[16] = module.charToString
module[17] = module.stringRelOp
module[18] = module.setSub
module[19] = bit.band
module[20] = module.setDiv
module[21] = module.setTest
module[22] = setmetatable
module[23] = module.is_a
module[24] = module.createSetArray
module[25] = module.println
module[26] = ffi.sizeof -- bytesize
module[27] = module.strcpy
module[28] = TRAP
module[29] = ASSERT
module[30] = module.removeElemFromSet
module[31] = math.ldexp
module[32] = module.UNPACK
module[33] = module.ODD
module[34] = math.abs
module[35] = bit.lshift
module[36] = bit.arshift
module[37] = bit.ror
module[38] = math.floor
module[39] = module.bool_to_number
module[40] = getmetatable
module[41] = bit.bxor
module[42] = ADDRESSOF
module[43] = module.createBoolArray
module[44] = ABORT
module[45] = C.ObxFfi_DBGTRACE -- or DBGTRACE
module[46] = C.ObxFfi_CRASH
module[47] = C.ObxFfi_TRACE -- or TRACE
module[48] = C.ObxFfi_NOP
module[49] = ffi.new
module[50] = ffi.copy
module[51] = module.min_size
module[52] = jit.off
module[53] = module.ldmod
module[54] = module.ldcmd

return module

