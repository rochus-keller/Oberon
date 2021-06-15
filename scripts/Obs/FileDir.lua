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
        int ObsFiles_listFiles();
        const char* ObsFiles_fileName( int i );
        uint32_t ObsFiles_fileSize( int i );
        uint32_t ObsFiles_fileTime( int i );
]]

local FileHeader = {}

-- PROCEDURE (name: FileName; fh: FileHeader; VAR continue: BOOLEAN);

function module.Enumerate(prefix,proc) -- prefix: ARRAY OF CHAR; proc: EntryHandler
	if fileSystemPath == nil then
		fileSystemPath = ""
	end
        local count = C.ObsFiles_listFiles()
	local fh = {}
	setmetatable(fh,FileHeader)
	for i = 0,count-1 do
                fh[0] = C.ObsFiles_fileSize(i) -- leng
                fh[1] = C.ObsFiles_fileTime(i) -- date
                local name = C.ObsFiles_fileName(i)
		name = obxlj.charToStringArray(32,name)
		local not_used, continue = proc(name,fh,true)
		if not continue then
			break
		end
	end
end

-- NOTE: these numbers are allocated by ObxLjbcGen and need to be updated if FileDir.Def changes!
module[0] = module.Enumerate
module[1] = FileHeader

FileDir = module -- make it globally visible

return module
