AddCSLuaFile()

E2Lib = {}

local type = type
local function checkargtype(argn, value, argtype)
	if type(value) ~= argtype then error(string.format("bad argument #%d to 'E2Lib.%s' (%s expected, got %s)", argn, debug.getinfo(2, "n").name, argtype, type(value)), 2) end
end

-- -------------------------- Helper functions -----------------------------
local unpack = unpack
local IsValid = IsValid

local tableInsert = table.insert
local tableConcat = table.concat
local tableRemove = table.remove
local tableToString = table.ToString

local stringMatch = string.match
local stringStartsWith = string.StartsWith
local stringFormat = string.format
local stringSub = string.sub

local StoreEntityModifier = duplicator.StoreEntityModifier

local mathClamp = math.Clamp
local mathHuge = math.huge
local mathFloor = math.floor
local mathMax = math.max

local isSinglePlayer = game.SinglePlayer()

-- This functions should not be used in functions that tend to be used very often, as it is slower than getting the arguments manually.
function E2Lib.getArguments(self, args)
	local ret = {}

	local count = #rawget(args, 7) + 1
	for i = 2, count do
	    local thisArg = rawget(args, i)
	    local first = rawget(thisArg, 1)

		rawset(ret, i - 1, first(self, thisAarg))
	end

	return unpack(ret)
end

-- Backwards compatibility
E2Lib.isnan = WireLib.isnan
E2Lib.clampPos = WireLib.clampPos
E2Lib.setPos = WireLib.setPos
E2Lib.setAng = WireLib.setAng

function E2Lib.setMaterial(ent, material)
	material = rawget(WireLib, "IsValidMaterial")(material)
	ent:SetMaterial(material)

	StoreEntityModifier(ent, "material", { MaterialOverride = material })
end

function E2Lib.setSubMaterial(ent, index, material)
	index = mathClamp(index, 0, 255)
	material = rawget(WireLib, "IsValidMaterial")(material)
	ent:SetSubMaterial(index, material)
	StoreEntityModifier(ent, "submaterial", { ["SubMaterialOverride_"..index] = material })
end

-- Returns a default e2 table.
function E2Lib.newE2Table()
	return {n={},ntypes={},s={},stypes={},size=0}
end

-- Returns a cloned table of the variable given if it is a table.
local istable = istable
local table_Copy = table.Copy
function E2Lib.fixDefault(var)
	return istable(var) and table_Copy(var) or var
end

-- getHash
-- Returns a hash for the given string

-- local str_byte = string.byte
-- local str_sub = string.sub
local util_CRC = util.CRC
local tonumber = tonumber
function E2Lib.getHash(self, data)
	--[[
	-- Thanks to emspike for this code
	self.prf = self.prf + #data
	local a, b = 1, 0
	for i = 1, #data do
			a = (a + str_byte(str_sub(data,i,i))) % 65521
			b = (b + a) % 65521
	end
	return b << 16 | a
	-- but we're going to use Garry's function, since it's most likely done in C++, so it's probably faster.
	-- For some reason, Garry's util.CRC returns a string... but it's always a number, so tonumbering it should work.
	-- I'm making it default to -1 if it for some reason throws a letter in there, breaking tonumber.
	]] --

	if self then
	    rawset(self, "prf", rawget(self, "prf") + #data / 10)
	end
	return tonumber(util_CRC(data)) or -1
end

-- -------------------------- signature generation -----------------------------

function E2Lib.typeName(typeid)
	if typeid == "" then return "void" end
	if typeid == "n" then return "number" end

	local tp = rawget(wire_expression_types2i, typeid)
	if not tp then error("Type ID '" .. typeid .. "' not found", 2) end

	local first = rawget(tp, 1)
	local typename = rawget(first, "lower")(first)
	return typename or "unknown"
end

function E2Lib.splitType(args)
	local thistype

	local ret = {}
    local retCount = 0

	local argsCount = #args
	local sub = rawget(args, "sub")
	local typeName = rawget(E2Lib, "typeName")

	local i = 1
	while i <= argsCount do
		local letter = sub(args, i, i)

		if letter == ":" then
			if retCount ~= 1 then error("Misplaced ':' in args", 2) end
			thistype = rawget(ret, 1)

			ret = {}
			retCount = 0

		elseif letter == "." then
			if sub(args, i) ~= "..." then error("Misplaced '.' in args", 2) end

			tableInsert(ret, "...")
			retCount = retCount + 1

			i = i + 2

		elseif letter == "=" then
			if retCount ~= 1 then error("Misplaced '=' in args", 2) end

			ret = {}
			retCount = 0

		else
			local typeid = letter
			if letter == "x" then
				typeid = sub(args, i, i + 2)
				i = i + 2
			end

			tableInsert(ret, typeName(typeid))
			retCount = retCount + 1
		end

		i = i + 1
	end

	return thistype, ret
end

-- given a function signature like "setNumber(xwl:sn)" and an optional return typeid, generates a nice, readable signature
function E2Lib.generate_signature(signature, rets, argnames)
	local funcname, args = stringMatch(signature, "([^(]+)%(([^)]*)%)")
	if not funcname then error("malformed signature") end

	local thistype, args = splitType(args)

	if argnames then
	    local argsCount = #args

		for i = 1, argsCount do
		    local argName = rawget(argnames, i)
			if argName then
			    local arg = rawget(args, i)
			    rawset(args, i, arg .. " " .. argName)
			end
		end
	end

	local new_signature = stringFormat("%s(%s)", funcname, tableConcat(args, ","))

	if thistype then
	    new_signature = thistype .. ":" .. new_signature
	end

	local emptyRets = not rets or rets == ""

	if emptyRets then
	    return new_signature
    else
        return rawget(E2Lib, "typeName")(rets) .. "=" .. new_signature
    end
end

-- ------------------------ various entity checkers ----------------------------

-- replaces an E2Lib function (ex.: isOwner) and notifies plugins
function E2Lib.replace_function(funcName, func)
	checkargtype(1, funcName, "string")
	checkargtype(2, func, "function")

	local oldfunc = rawget(E2Lib, funcName)

	if not isfunction(oldfunc) then
	    error("No E2Lib function by the name " .. funcName .. " found.", 2)
	end

	rawset(E2Lib, funcName, func)
	wire_expression2_CallHook("e2lib_replace_function", funcName, func, oldfunc)
end

function E2Lib.validPhysics(entity)
	if IsValid(entity) then
		if entity:IsWorld() then return false end
		if entity:GetMoveType() ~= MOVETYPE_VPHYSICS then return false end
		return entity:GetPhysicsObject():IsValid()
	end

	return false
end

-- This function gets wrapped when CPPI is detected, see very end of this file
function E2Lib.getOwner(self, entity)
	if entity == nil then return end

	local selfEntity = rawget(self, "entity")
	local selfPlayer = rawget(self, "player")

	if entity == selfEntity or entity == selfPlayer then
	    return selfPlayer
	end

	local entityGetPlayer = entity.GetPlayer
	if entityGetPlayer then
		local ply = entityGetPlayer(entity)
		if IsValid(ply) then return ply end
	end

	local OnDieFunctions = entity.OnDieFunctions

	if OnDieFunctions then
	    local getCountUpdate = rawget(OnDieFunctions, "GetCountUpdate")

		if getCountUpdate then
		    local getCountUpdateArgs = rawget(getCountUpdate, "Args")

			if getCountUpdateArgs then
			    local firstArg = rawget(getCountUpdateArgs, 1)
				if firstArg then return firstArg end
			end
		end

		local undo1 = rawget(OnDieFunctions, "undo1")
		if undo1 then
		    local undo1Args = rawget(undo1, "Args")

			if undo1Args then
			    local secondArg = rawget(undo1Args, 2)
				if secondArg then return secondArg end
			end
		end
	end

	local entityGetOwner = entity.GetOwner
	if entityGetOwner then
		local ply = entityGetOwner(entity)
		if IsValid(ply) then return ply end
	end

	return nil
end

function E2Lib.abuse(ply)
	ply:Kick("Be good and don't abuse -- sincerely yours, the E2")
	error("abuse", 0)
end

-- This function gets replaced when CPPI is detected, see very end of this file
function E2Lib.isFriend(owner, player)
	return owner == player
end

function E2Lib.isOwner(self, entity)
	if isSinglePlayer then return true end

	local owner = rawget(E2Lib, "getOwner")(self, entity)

	if not IsValid(owner) then return false end

	local player = rawget(self, "player")
	return rawget(E2Lib, "isFriend")(owner, player)
end

local isOwner = E2Lib.isOwner

-- Checks whether the player is the chip's owner or in a pod owned by the chip's owner. Assumes that ply is really a player.
function E2Lib.canModifyPlayer(self, ply)
	if ply == rawget(self, "player") then return true end

	if not IsValid(ply) then return false end
	if not ply:IsPlayer() then return false end

	local vehicle = ply:GetVehicle()
	if not IsValid(vehicle) then return false end
	return isOwner(self, vehicle)
end

-- ------------------------ type guessing ------------------------------------------

local type_lookup = {
	number = "n",
	string = "s",
	Vector = "v",
	PhysObj = "b",
}
local table_length_lookup = {
	[2] = "xv2",
	[3] = "v",
	[4] = "xv4",
	[9] = "m",
	[16] = "xm4",
}

function E2Lib.guess_type(value)
	local vtype = type(value)
	local vtypeLookup = rawget(type_lookup, vtype)
	if vtypeLookup then return vtypeLookup end

	if IsValid(value) then return "e" end
	if rawget(value, "EntIndex") then return "e" end

	if vtype == "table" then
	    local val = rawget(table_length_lookup, #value)
	    if val then return val end
		if rawget(value, "HitPos") then return "xrd" end
	end

	for typeid, v in pairs(wire_expression_types2) do
	    local fifth = rawget(v, 5)

		if fifth then
			local ok = pcall(fifth, value)
			if ok then return typeid end
		end
	end

	-- TODO: more type guessing here

	return "" -- empty string = unknown type, for now.
end

-- Types that cannot possibly be guessed correctly:
-- angle (will be reported as vector)
-- matrix2 (will be reported as vector4)
-- wirelink (will be reported as entity)
-- complex (will be reported as vector2)
-- quaternion (will be reported as vector4)
-- all kinds of nil stuff

-- ------------------------ list filtering -------------------------------------------------

function E2Lib.filterList(list, criterion)
	local listSize = #list

	for i = listSize, 1, -1 do
	    local item = rawget(list, i)

	    if not criterion(item) then
	        tableRemove( list, i )
        end
    end

	return list
end

-- ----------------------------- compiler stuf ---------------------------------

-- A function suitable for use as xpcall's error handler. If the error is
-- generated by Compiler:Error, Parser:Error, etc., then the string will be a
-- usable error message. If not, then it's an error not caused by an error in
-- user code, and so we dump a stack trace to the console to help debug it.
function E2Lib.errorHandler(message)
	if stringStartsWith(message, "[ERROR] ") then return message end

	print("Internal error - please report to https://github.com/wiremod/wire/issues")
	print(message)
	debug.Trace()
	return "Internal error, see console for more details"
end

E2Lib.optable_inv = {
	add = "+",
	sub = "-",
	mul = "*",
	div = "/",
	mod = "%",
	exp = "^",
	ass = "=",
	aadd = "+=",
	asub = "-=",
	amul = "*=",
	adiv = "/=",
	inc = "++",
	dec = "--",
	eq = "==",
	neq = "!=",
	lth = "<",
	geq = ">=",
	leq = "<=",
	gth = ">",
	band = "&&",
	bor = "||",
	bxor = "^^",
	bshr = ">>",
	bshl = "<<",
	["not"] = "!",
	["and"] = "&",
	["or"] = "|",
	qsm = "?",
	col = ":",
	def = "?:",
	com = ",",
	lpa = "(",
	rpa = ")",
	lcb = "{",
	rcb = "}",
	lsb = "[",
	rsb = "]",
	dlt = "$",
	trg = "~",
	imp = "->",
}

E2Lib.optable = {}
for token, op in pairs(E2Lib.optable_inv) do
	local current = E2Lib.optable
	for i = 1, #op do
		local c = op:sub(i, i)
		local nxt = current[c]
		if not nxt then
			nxt = {}
			current[c] = nxt
		end

		if i == #op then
			nxt[1] = token
		else
			if not nxt[2] then
				nxt[2] = {}
			end

			current = nxt[2]
		end
	end
end

local op_order = { ["+"] = 1, ["-"] = 2, ["*"] = 3, ["/"] = 4, ["%"] = 5, ["^"] = 6, ["="] = 7, ["!"] = 8, [">"] = 9, ["<"] = 10, ["&"] = 11, ["|"] = 12, ["?"] = 13, [":"] = 14, [","] = 15, ["("] = 16, [")"] = 17, ["{"] = 18, ["}"] = 19, ["["] = 20, ["]"] = 21, ["$"] = 22, ["~"] = 23 }
function E2Lib.printops()
	print("E2Lib.optable = {")

	local optable = rawget(E2Lib, "optable")
	local sortFunc = function(a, b)
	    return (rawget(op_orders, a) or mathHuge) < (rawget(op_orders, b) or mathHuge)
    end

	for k, v in pairs_sortkeys(optable, sortFunc) do
		local tblstring = tableToString(v)
		local gsub = tblstring.gsub

		tblstring = gsub(tblstring, ",}", "}")
		tblstring = gsub(tblstring, "{(.)=", " {[\"%1\"] = ")
		tblstring = gsub(tblstring, ",(.)=", ", [\"%1\"] = ")
		print(stringFormat("\t[%q] = %s,", k, tblstring))
	end

	print("}")
end

-- ------------------------------ string stuff ---------------------------------

-- limits the given string to the given length and adds "..." to the end if too long.
function E2Lib.limitString(text, length)
	checkargtype(1, text, "string")
	checkargtype(2, length, "number")

	if #text <= length then
		return text
	else
		return stringSub(text, 1, length) .. "..."
	end
end

do
	local enctbl = {}
	local dectbl = {}

	do
		-- generate encode/decode lookup tables
		-- local valid_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 +-*/#^!?~=@&|.,:(){}[]<>" -- list of "normal" chars that can be transferred without problems
		local invalid_chars = "'\"\n\\%"
		local hex = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' }


		for i = 1, #invalid_chars do
			local char = invalid_chars:sub(i, i)
			enctbl[char] = true
		end
		for byte = 1, 255 do
			dectbl[hex[(byte - byte % 16) / 16 + 1] .. hex[byte % 16 + 1]] = string.char(byte)
			if enctbl[string.char(byte)] then
				enctbl[string.char(byte)] = "%" .. hex[(byte - byte % 16) / 16 + 1] .. hex[byte % 16 + 1]
			else
				enctbl[string.char(byte)] = string.char(byte)
			end
		end

		--for i = 1, #valid_chars do
		--	local char = valid_chars:sub(i, i)
		--	enctbl[char] = char
		--end
	end

	-- escapes special characters
	function E2Lib.encode(str)
	    return str:gsub(".", enctbl)
	end

	-- decodes escaped characters
	function E2Lib.decode(encoded)
		return encoded:gsub("%%(..)", dectbl)
	end
end

-- ------------------------------- extensions ----------------------------------

do
	-- Shared stuff, defined later.

	local extensions, printExtensions, conCommandSetExtensionStatus

	function E2Lib.GetExtensions()
		return rawget(extensions, "list")
	end

	function E2Lib.GetExtensionStatus(name)
	    name = name:Trim()
	    name = name:lower()
		return rawget(rawget(extensions, "status"), name)
	end

	function E2Lib.GetExtensionDocumentation(name)
		return rawget(rawget(extensions, "documentation"), name) or {}
	end

	if SERVER then -- serverside stuff
	    local addNetworkString = rawget(util, "AddNetworkString")

		addNetworkString( "wire_expression2_server_send_extensions_list" )
		addNetworkString( "wire_expression2_client_request_print_extensions" )
		addNetworkString( "wire_expression2_client_request_set_extension_status" )

		function wire_expression2_PreLoadExtensions()
			rawget(hook, "Run")( "Expression2_PreLoadExtensions" )
			local query = rawget(sql, "Query")

			extensions = { status = {}, list = {}, prettyList = {}, documentation = {} }
			local list = query( "SELECT * FROM wire_expression2_extensions" )

			if list then
			    local listCount = #list

				for i = 1, listCount do
					local row = rawget(list, i)
					rawget(E2Lib, "SetExtensionStatus")( rawget(row, "name"), rawget(row, "enabled"))
				end

			else
				query( "CREATE TABLE wire_expression2_extensions ( name VARCHAR(32) PRIMARY KEY, enabled BOOLEAN )" )
			end

			rawset(extensions, "save", true)
		end

		function E2Lib.RegisterExtension(name, default, description, warning)
			name = name:Trim():lower()

			if extensions.status[ name ] == nil then
				E2Lib.SetExtensionStatus( name, default )
			end

			extensions.list[ #extensions.list + 1 ] = name

			if description or warning then
				extensions.documentation[name] = { Description = description, Warning = warning }
			end

			-- This line shouldn't be modified because it tells the parser that this extension is disabled,
			-- thus making its functions not available in the E2 Editor (see function e2_include_pass2 in extloader.lua).
			assert( extensions.status[ name ], "EXTENSION_DISABLED" )
		end

		function E2Lib.SetExtensionStatus( name, status )
			name = name:Trim():lower()
			status = tobool( status )
			extensions.status[ name ] = status
			if extensions.save then
				sql.Query( "REPLACE INTO wire_expression2_extensions ( name, enabled ) VALUES ( " .. sql.SQLStr( name ) .. ", " .. ( status and 1 or 0 ) .. " )" )
			end
		end

		-- After using E2Lib.SetExtensionStatus in an external script, this function should be called.
		-- Its purpose is to update the clientside autocomplete list for the concommands.
		function E2Lib.UpdateClientsideExtensionsList( ply )
			net.Start( "wire_expression2_server_send_extensions_list" )
			net.WriteTable(extensions)
			if IsValid( ply ) then
				net.Send( ply )
			else
				net.Broadcast()
			end
		end

		local function buildPrettyList()
		    local rep = (" ").rep

			local function padLeft( str, len ) return rep( " ", len - #str ) .. str end
			local function padRight( str, len ) return str .. rep( " ", len - #str ) end
			local function padCenter( str, len ) return padRight( padLeft( str, mathFloor( (len + #str) / 2 ) ), len ) end

			local list, column1, column2, columnsWidth = rawget(extensions, "list"), {}, {}, 0

			local listCount = #list
			for i = 1, listCount do
				local name = rawget(list, i)

				if #name > columnsWidth then columnsWidth = #name end

				if rawget(rawget(extensions, "status"), name) == true then
				    rawset(column1, #column1 + 1, name)
				else
				    rawset(column2, #column2 + 1, name)
				end
			end

			local mainTitle, column1Title, column2Title = "E2 EXTENSIONS", "ENABLED", "DISABLED"
			local maxWidth, maxRows = mathMax( columnsWidth * 2, #column1Title + #column2Title, #mainTitle - 3 ), mathMax( #column1, #column2 )

			if maxWidth % 2 ~= 0 then maxWidth = maxWidth + 1 end

			columnsWidth = maxWidth / 2
			maxWidth = maxWidth + 3

			local delimiter =  " +-" .. rep( "-", columnsWidth ) .. "-+-" .. rep( "-", columnsWidth ) .. "-+"

			list = {
				" +-" .. rep( "-", maxWidth ) .. "-+",
				" | " .. padCenter( mainTitle, maxWidth ) .. " |",
				delimiter,
				" | " .. padCenter( column1Title, columnsWidth ) .. " | " .. padCenter( column2Title, columnsWidth ) .. " |",
				delimiter,
			}
			for i = 1, maxRows do
			   rawset(list, #list + 1, " | " .. padRight( rawget(column1, i) or "", columnsWidth ) .. " | " .. padRight( rawget(column2, i) or "", columnsWidth ) .. " |")
			end

			rawset(list, #list + 1, delimiter)

			rawset(extensions, "prettyList", list)
		end

		function printExtensions( ply, str )
            local prettyList = rawget( extensions, "prettyList" )
            local prettyListCount = #prettyList

			if IsValid( ply ) then
			    local printMessage = ply.PrintMessage
				if str then printMessage( ply, 2, str ) end

				for i = 1, #prettyList do
				    printMessage( ply, 2, rawget( prettyList, i ) )
				end
			else
				if str then print( str ) end
				for i = 1, prettyListCount do print( rawget( prettyList, i ) ) end
			end
		end

		function conCommandSetExtensionStatus( ply, cmd, args )
			if IsValid( ply ) and not ply:IsSuperAdmin() and not game.SinglePlayer() then
				ply:PrintMessage( 2, "Sorry " .. ply:Name() .. ", you don't have access to this command." )
				return
			end
			local name = args[ 1 ]
			if name then
				name = name:Trim():lower()
				if extensions.status[ name ] ~= nil then
					local status = tobool( cmd:find( "enable" ) )
					if extensions.status[ name ] == status then
						local str = "Extension '" .. name .. "' is already " .. ( status and "enabled" or "disabled" ) .. "."
						if IsValid( ply ) then ply:PrintMessage( 2, str ) else print( str ) end
					else
						E2Lib.SetExtensionStatus( name, status )
						E2Lib.UpdateClientsideExtensionsList()
						local str = "E2 Extension '" .. name .. "' has been " .. ( status and "enabled" or "disabled" )
						if not game.SinglePlayer() and IsValid( ply ) then MsgN( str .. " by " .. ply:Name() .. " (" .. ply:SteamID() .. ")." ) end
						local canReloadNow = #player.GetAll() == 0
						if canReloadNow then str = str .. ". Expression 2 will be reloaded now."  else str = str .. ". Expression 2 will be reloaded in 10 seconds." end
						if IsValid( ply ) then ply:PrintMessage( 2, str ) else print( str ) end
						if canReloadNow then wire_expression2_reload( ply ) else timer.Create( "E2_AutoReloadTimer", 10, 1, function() wire_expression2_reload( ply ) end ) end
					end
				else printExtensions( ply, "Unknown extension '" .. name .. "'. Here is a list of available extensions:" ) end
			else printExtensions( ply, "Usage: '" .. cmd .. " <name>'. Here is a list of available extensions:" ) end
		end

		net.Receive( "wire_expression2_client_request_print_extensions",
			function( _, ply )
				printExtensions( ply )
			end
		)

		net.Receive( "wire_expression2_client_request_set_extension_status",
			function( _, ply )
				conCommandSetExtensionStatus( ply, net.ReadString(), net.ReadTable() )
			end
		)

		hook.Add( "PlayerInitialSpawn", "wire_expression2_updateClientsideExtensions", E2Lib.UpdateClientsideExtensionsList )

		function wire_expression2_PostLoadExtensions()
			table.sort( extensions.list, function( a, b ) return a < b end )

			E2Lib.UpdateClientsideExtensionsList()
			buildPrettyList()

			if not wire_expression2_is_reload then -- only print once on startup, not on each reload.
				printExtensions()
			end
			hook.Run( "Expression2_PostLoadExtensions" )
		end

	else -- clientside stuff

		extensions = { status = {}, list = {} }

		function printExtensions()
			net.Start( "wire_expression2_client_request_print_extensions" )
			net.SendToServer()
		end

		function conCommandSetExtensionStatus( _, cmd, args )
			net.Start( "wire_expression2_client_request_set_extension_status" )
			net.WriteString( cmd )
			net.WriteTable( args )
			net.SendToServer()
		end

		net.Receive( "wire_expression2_server_send_extensions_list", function()
			extensions = net.ReadTable()
		end)

	end

	-- shared stuff

	local function makeAutoCompleteList( cmd, args )
	    local trimmed = args:Trim()
	    local args = trimmed:lower()

		local status, list, tbl, j = tobool( cmd:find( "enable" ) ), rawget(extensions, "list"), {}, 1

		local listCount = #list
		for i = 1, listCount do
			local name = rawget(list, i)
			local status = rawget(extensions, "status")

			if rawget( status, name ) ~= status and name:find( args ) then
			    rawset(tbl, j, cmd .. " " .. name)
				j = j + 1
			end
		end

		return tbl
	end

	concommand.Add( "wire_expression2_extension_enable", conCommandSetExtensionStatus, makeAutoCompleteList )
	concommand.Add( "wire_expression2_extension_disable", conCommandSetExtensionStatus, makeAutoCompleteList )
	concommand.Add( "wire_expression2_extensions", function( ply ) printExtensions( ply ) end )

end

-- ------------------------ clientside reload command --------------------------

do
	if SERVER then

		util.AddNetworkString( "wire_expression2_client_request_reload" )
		net.Receive( "wire_expression2_client_request_reload",
			function( n, ply )
				wire_expression2_reload( ply )
			end
		)

	else

		local function wire_expression2_reload()
			net.Start( "wire_expression2_client_request_reload" )
			net.SendToServer()
		end

		concommand.Add( "wire_expression2_reload", wire_expression2_reload )

	end

end

-- ------------------------------ compatibility --------------------------------

-- Some functions need to be global for backwards-compatibility.
local makeglobal = {
	["validPhysics"] = true,
	["getOwner"] = true,
	["isOwner"] = true,
}

-- Put all these functions into the global scope.
for funcname, _ in pairs(makeglobal) do
	_G[funcname] = E2Lib[funcname]
end

hook.Add("InitPostEntity", "e2lib", function()
-- If changed, put them into the global scope again.
	registerCallback("e2lib_replace_function", function(funcname, func, oldfunc)
		if makeglobal[funcname] then
			_G[funcname] = func
		end
		if funcname == "IsValid" then IsValid = func
		elseif funcname == "isOwner" then isOwner = func
		end
	end)

	-- check for a CPPI compliant plugin
	if SERVER and CPPI then
		if debug.getregistry().Player.CPPIGetFriends then
			E2Lib.replace_function("isFriend", function(owner, player)
				if owner == nil then return false end
				if owner == player then return true end

				local friends = owner:CPPIGetFriends()
				if not istable(friends) then return end

				local friendCount = #friends
				for i = 1, friendCount do
				    local friend = rawget(friends, i)
					if player == friend then return true end
				end

				return false
			end)
		end

		if debug.getregistry().Entity.CPPIGetOwner then
			local _getOwner = E2Lib.getOwner

			E2Lib.replace_function("getOwner", function(self, entity)
				if not IsValid(entity) then return end
				local selfPlayer = rawget(self, "player")

				if entity == rawget(self, "entity") or entity == selfPlayer then
				    return selfPlayer
				end

				local owner = entity:CPPIGetOwner()
				if IsValid(owner) then return owner end

				return _getOwner(self, entity)
			end)
		end
	end
end)
