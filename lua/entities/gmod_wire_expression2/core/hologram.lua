E2Lib.RegisterExtension( "holo", true, "Allows E2 to create and manipulate non-solid models." )

-- -----------------------------------------------------------------------------

-- TODO: short-term checkOwner cache maybe?
local function checkOwner(self)
	return IsValid(rawget(self, player))
end

-- -----------------------------------------------------------------------------

local wire_holograms_max = CreateConVar( "wire_holograms_max", "250", {FCVAR_ARCHIVE} )
local wire_holograms_spawn_amount = CreateConVar( "wire_holograms_spawn_amount", "15", {FCVAR_ARCHIVE} ) -- This limit resets once a second
local wire_holograms_burst_amount = CreateConVar( "wire_holograms_burst_amount", "80", {FCVAR_ARCHIVE} ) -- This limit goes down first, resets every burst_delay
local wire_holograms_burst_delay = CreateConVar( "wire_holograms_burst_delay", "10", {FCVAR_ARCHIVE} )
local wire_holograms_max_clips = CreateConVar( "wire_holograms_max_clips", "5", {FCVAR_ARCHIVE} ) -- Don't set higher than 16 without editing net.Start("wire_holograms_clip")
local wire_holograms_modelany = CreateConVar( "wire_holograms_modelany", "0", {FCVAR_ARCHIVE},
	"1: Allow holograms to use models besides the official hologram models." ..
	"2: Allow holograms to additionally use models not present on the server." )
local wire_holograms_size_max = CreateConVar( "wire_holograms_size_max", "50", {FCVAR_ARCHIVE} )
util.AddNetworkString("wire_holograms_set_visible")
util.AddNetworkString("wire_holograms_clip")
util.AddNetworkString("wire_holograms_set_scale")
util.AddNetworkString("wire_holograms_set_bone_scale")
util.AddNetworkString("wire_holograms_set_player_color")

local tableRemove = table.remove
local tableInsert = table.insert
local netStart = net.Start
local netWriteUInt = net.WriteUInt
local netWriteFloat = net.WriteFloat
local netWriteBit = net.WriteBit
local netWriteVector = net.WriteVector
local netSend = net.Send
local netBroadcast = net.Broadcast

local mathClamp = math.Clamp

-- context = chip.context = self
-- uid = context.uid = self.uid = chip.uid = player:UniqueID()
-- Holo = { ent = prop, scale = scale, e2owner = context }
-- E2HoloRepo[uid][-index] = Holo <-- global holos
-- E2HoloRepo[uid][Holo] = Holo <-- local holos
-- context.data.holos[index] = Holo <-- local holos

local E2HoloRepo = {}
local PlayerAmount = {}
local BlockList = {}

local ModelList = {
	["cone"]              = "cone",
	["cplane"]			  = "cplane",
	["cube"]              = "cube",
	["cylinder"]          = "cylinder",
	["hq_cone"]           = "hq_cone",
	["hq_cylinder"]       = "hq_cylinder",
	["hq_dome"]           = "hq_dome",
	["hq_hdome"]          = "hq_hdome",
	["hq_hdome_thick"]    = "hq_hdome_thick",
	["hq_hdome_thin"]     = "hq_hdome_thin",
	["hq_icosphere"]      = "hq_icosphere",
	["hq_sphere"]         = "hq_sphere",
	["hq_torus"]          = "hq_torus",
	["hq_torus_thick"]    = "hq_torus_thick",
	["hq_torus_thin"]     = "hq_torus_thin",
	["hq_torus_oldsize"]  = "hq_torus_oldsize",
	["hq_tube"]           = "hq_tube",
	["hq_tube_thick"]     = "hq_tube_thick",
	["hq_tube_thin"]      = "hq_tube_thin",
	["hq_stube"]          = "hq_stube",
	["hq_stube_thick"]    = "hq_stube_thick",
	["hq_stube_thin"]     = "hq_stube_thin",
	["icosphere"]         = "icosphere",
	["icosphere2"]        = "icosphere2",
	["icosphere3"]        = "icosphere3",
	["plane"]             = "plane",
	["prism"]             = "prism",
	["pyramid"]           = "pyramid",
	["sphere"]            = "sphere",
	["sphere2"]           = "sphere2",
	["sphere3"]           = "sphere3",
	["tetra"]             = "tetra",
	["torus"]             = "torus",
	["torus2"]            = "torus2",
	["torus3"]            = "torus3",

	["rcube"]             = "rcube",
	["rcube_thick"]       = "rcube_thick",
	["rcube_thin"]     	  = "rcube_thin",
	["hq_rcube"]          = "hq_rcube",
	["hq_rcube_thick"]    = "hq_rcube_thick",
	["hq_rcube_thin"]     = "hq_rcube_thin",
	["rcylinder"]         = "rcylinder",
	["rcylinder_thick"]   = "rcylinder_thick",
	["rcylinder_thin"]    = "rcylinder_thin",
	["hq_rcylinder"]      = "hq_rcylinder",
	["hq_rcylinder_thick"]= "hq_rcylinder_thick",
	["hq_rcylinder_thin"] = "hq_rcylinder_thin",
	["hq_cubinder"]       = "hq_cubinder",
	["hexagon"]           = "hexagon",
	["octagon"]           = "octagon",
	["right_prism"]       = "right_prism",

	-- Removed models with their replacements

	["dome"]             = "hq_dome",
	["dome2"]            = "hq_hdome",
	["hqcone"]           = "hq_cone",
	["hqcylinder"]       = "hq_cylinder",
	["hqcylinder2"]      = "hq_cylinder",
	["hqicosphere"]      = "hq_icosphere",
	["hqicosphere2"]     = "hq_icosphere",
	["hqsphere"]         = "hq_sphere",
	["hqsphere2"]        = "hq_sphere",
	["hqtorus"]          = "hq_torus_oldsize",
	["hqtorus2"]         = "hq_torus_oldsize",

	-- HQ models with their short names

	["hqhdome"]          = "hq_hdome",
	["hqhdome2"]         = "hq_hdome_thin",
	["hqhdome3"]         = "hq_hdome_thick",
	["hqtorus3"]         = "hq_torus_thick",
	["hqtube"]           = "hq_tube",
	["hqtube2"]          = "hq_tube_thin",
	["hqtube3"]          = "hq_tube_thick",
	["hqstube"]          = "hq_stube",
	["hqstube2"]         = "hq_stube_thin",
	["hqstube3"]         = "hq_stube_thick",
	["hqrcube"]          = "hq_rcube",
	["hqrcube2"]         = "hq_rcube_thick",
	["hqrcube3"]         = "hq_rcube_thin",
	["hqrcylinder"]      = "hq_rcylinder",
	["hqrcylinder2"]     = "hq_rcylinder_thin",
	["hqrcylinder3"]     = "hq_rcylinder_thick",
	["hqcubinder"]       = "hq_cubinder"
}

-- Return the absolute model path
local function modelPath(name)
	return "models/holograms/" .. name .. ".mdl"
end

local added = {}
local pathLookup = {}

for _,v in pairs( ModelList ) do
	if not added[v] then
		local path = modelPath(v)

		pathLookup[path] = true
		util.PrecacheModel(path)
		-- resource.AddSingleFile( "models/holograms/" .. v .. ".mdl" )

		added[v] = true
	end
end

local wirelibCanModel
local function GetModel(self, model, skin)
	skin = skin or 0

	local modelFromList = rawget(ModelList, model)
	local modelAny = wire_holograms_modelany:GetInt()

	if modelFromList then
		model = modelFromList

	-- If this model isn't already the absolute path of a default model, and only default models are allowed
	elseif not rawget(pathLookup, model) and modelAny == 0 then
		return false
	end

	local canModel = wirelibCanModel
	if not canModel then
	    wirelibCanModel = rawget(WireLib, "CanModel")
	    canModel = wirelibCanModel
	end

	local ply = rawget(self, "player")

	if modelAny ~= 2 and not canModel(ply, model, skin) then
		-- Check if the model is at least valid
		if not canModel(ply, model, 0) then
			return false
		end

		-- The model was valid however the skin was not. Go to default skin
		skin = 0
	end

	return model, skin
end

-- -----------------------------------------------------------------------------

local scale_queue = {}
local bone_scale_queue = {}
local clip_queue = {}
local vis_queue = {}
local player_color_queue = {}

local function add_queue( queue, ply, data )
	local plyqueue = rawget(queue, ply)

	if not plyqueue then
		plyqueue = {}
		rawset(queue, ply, plyqueue)
	end

	local plyQueueSize = #plyqueue
	if plyQueueSize == wire_holograms_max:GetInt() then return end

	tableInsert( plyqueue, data )
end

local function remove_from_queue( queue, holo_ent )
    local queueSize = #queue

    for i = 1, queueSize do
        local plyqueue = rawget(queue, i)
        local plyqueueSize = #plyqueue

        for j = plyqueueSize, 1, -1 do -- iterate backwards to allow removing
            -- local holo = plyqueue[j][1] -- the hologram is always at idx 1

            local holo = rawget( rawget(plyqueue, j), 1 )
            local holoEnt = rawget( holo, "ent" )

            if holoEnt == holo_ent then
                tableRemove( plyqueue, i ) -- remove it from the queue
            end
        end
    end
end

-- call to remove all queued items for a specific hologram
local function remove_from_queues( holo_ent )
	remove_from_queue( scale_queue, holo_ent )
	remove_from_queue( bone_scale_queue, holo_ent )
	remove_from_queue( clip_queue, holo_ent )
	remove_from_queue( vis_queue, holo_ent )
	remove_from_queue( player_color_queue, holo_ent )
end

local function remove_holo( Holo )
    local holoEnt = rawget( Holo, "ent" )

	if IsValid(holoEnt) then
		remove_from_queues( holoEnt )
		holoEnt:Remove()
	end
end

local function flush_scale_queue(queue, recipient)
	if not queue then queue = scale_queue end
	if not next(queue) then return end

	netStart("wire_holograms_set_scale")
	    local queueSize = #queue

	    for i = 1, queueSize do
	        local plyqueue = rawget( queue, i )

			for _,Holo,scale in ipairs_map(plyqueue, unpack) do
				netWriteUInt(rawget(Holo, "ent"):EntIndex(), 16)
				netWriteFloat(rawget(scale, "x"))
				netWriteFloat(rawget(scale, "y"))
				netWriteFloat(rawget(scale, "z"))
			end
		end

		netWriteUInt(0, 16)

	if recipient then netSend(recipient) else netBroadcast() end
end

local function flush_bone_scale_queue(queue, recipient)
	if not queue then queue = bone_scale_queue end
	if not next(queue) then return end

	net.Start("wire_holograms_set_bone_scale")
	    local queueSize = #queue

        for i = 1, queueSize do
            local plyqueue = rawget(queue, i)

            for _,Holo,bone,scale in ipairs_map(plyqueue, unpack) do

                netWriteUInt(rawget(Holo, "ent"):EntIndex(), 16)
                netWriteUInt(bone + 1, 16) -- using +1 to be able reset holo bones scale with -1 and not use signed int
                netWriteFloat(rawget(scale, "x"))
                netWriteFloat(rawget(scale, "y"))
                netWriteFloat(rawget(scale, "z"))

            end
        end

        netWriteUInt(0, 16)
        netWriteUInt(0, 16)

	if recipient then netSend(recipient) else netBroadcast() end
end

local function flush_clip_queue(queue, recipient)
	if not queue then queue = clip_queue end
	if not next(queue) then return end

	net.Start("wire_holograms_clip")
	    local queueSize = #queue

	    for i = 1, queueSize do
	        local plyqueue = rawget(queue, i)

			for _, Holo, clip in ipairs_map(plyqueue, unpack) do

				if clip and rawget(clip, "index") then
					netWriteUInt(rawget(Holo, "ent"):EntIndex(), 16)
					netWriteUInt(rawget(clip, "index"), 4) -- 4: absolute highest wire_holograms_max_clips is thus 16

					local clipEnabled = rawget(clip, "enabled")
					local clipOrigin = rawget(clip, "origin")
					local clipNormal = rawget(clip, "normal")
					local clipLocalEntId = rawget(clip, "localentid")

					if clipEnabled ~= nil then
						netWriteBit(true)
						netWriteBit(clipEnabled)

					elseif clipOrigin and clipNomral and clipLocalEntId then
						netWriteBit(false)
						netWriteVector(clipOrigin)
						netWriteFloat(rawget(clipNormal, "x"))
						netWriteFloat(rawget(clipNormal, "y"))
						netWriteFloat(rawget(clipNormal, "z"))
						netWriteUInt(clipLocalEntId, 16)
					end
				end

			end
		end

		netWriteUInt(0, 16)

	if recipient then netSend(recipient) else netBroadcast() end
end

local function flush_vis_queue()
	if not next(vis_queue) then return end

	for ply,tbl in pairs( vis_queue ) do
		if IsValid( ply ) and #tbl > 0 then
			netStart("wire_holograms_set_visible")

				for _,Holo,visible in ipairs_map(tbl, unpack) do
					netWriteUInt(rawget(Holo, "ent"):EntIndex(), 16)
					netWriteBit(visible)
				end

				netWriteUInt(0, 16)
			netSend(ply)
		end
	end
end

local function flush_player_color_queue()
	if not next(player_color_queue) then return end

	netStart("wire_holograms_set_player_color")
		for _, plyqueue in pairs(player_color_queue) do

			for _,Holo,color in ipairs_map(plyqueue, unpack) do
			    local holoEnt = rawget(Holo, "ent")
				netWriteUInt(holoEnt:EntIndex(),  16)
				netWriteVector(color)
			end

		end

		netWriteUInt(0, 16)

	netBroadcast()
end

registerCallback("postexecute", function(self)
	flush_scale_queue()
	flush_bone_scale_queue()
	flush_clip_queue()
	flush_vis_queue()
	flush_player_color_queue()

	scale_queue = {}
	bone_scale_queue = {}
	clip_queue = {}
	vis_queue = {}
	player_color_queue = {}
end)

local function rescale(Holo, scale, bone)
	local maxval = wire_holograms_size_max:GetInt()
	local minval = -maxval

	if scale then
		local x = mathClamp( rawget(scale, 1), minval, maxval )
		local y = mathClamp( rawget(scale, 2), minval, maxval )
		local z = mathClamp( rawget(scale, 3), minval, maxval )
		local scale = Vector(x, y, z)

		if rawget(Holo, "scale") ~= scale then
			add_queue( scale_queue, rawget(Holo, "e2owner"), { Holo, scale } )
			rawset(Holo, "scale", scale)
		end
	end

	if bone then
	    local boneScale = rawget(Holo, "bone_scale")
	    rawset(Holo, "bone_scale", boneScale or {})

		if #bone == 2 then
			local bidx, b_scale = rawget(bone, 1), rawget(bone, 2)

			local x = mathClamp( rawget(b_scale, 1), minval, maxval )
			local y = mathClamp( rawget(b_scale, 2), minval, maxval )
			local z = mathClamp( rawget(b_scale, 3), minval, maxval )
			local scale = Vector(x, y, z)

			add_queue( bone_scale_queue, rawget(Holo, "e2owner"), { Holo, bidx, scale } )
			Holo.bone_scale[bidx] =  scale
		else  -- reset holo bone scale
			add_queue( bone_scale_queue, rawget(Holo, "e2owner"), { Holo, -1, Vector(0,0,0) } )
			Holo.bone_scale = {}
		end
	end
end

local function check_clip(Holo, idx)
	local holoClips = rawget(Holo, "clips") or {}
	rawset(Holo, "clips", holoClips)

	if idx > 0 and idx <= wire_holograms_max_clips:GetInt() then

		Holo.clips[idx] = Holo.clips[idx] or {}
		local clips = rawget(Holo, "clips")
		local clip = rawget(clips, idx)

		local clipEnabled = rawget(clip, "enabled") or false
		rawset(clip, "enabled", clipEnabled)

		local clipOrigin = rawget(clip, "origin") or Vector(0, 0, 0)
		rawset(clip, "origin", clipOrigin)

		local clipNormal = rawget(clip, "normal") or Vector(0, 0, 0)
		rawset(clip, "normal", clipNormal)

		local clipLocalEntId = rawget(clip, "localentid") or 0
		rawset(clip, "localentid", clipLocalEntId)

		return clip
	end

	return nil
end

local function enable_clip(Holo, idx, enabled)
	local clip = check_clip(Holo, idx)
	local clipEnabled = rawget(clip, "enabled")

	if clip and clipEnabled ~= enabled then
	    rawset(clip, "enabled", enabled)

		add_queue( clip_queue, rawget(Holo, "e2owner"), { Holo,
			{
				index = idx,
				enabled = enabled
			}}
		)
	end
end

local function set_clip(Holo, idx, origin, normal, localentid)
	local clip = check_clip(Holo, idx)

	local clipOrigin = rawget(clip, "origin")
	local clipNormal = rawget(clip, "normal")
	local clipLocalentid = rawget(clip, "localentid")

	if not clip then return end

	if clipOrigin ~= origin or clipNormal ~= normal or clipLocalentid ~= localentid then
	    rawset(clip, "origin", origin)
		clip.origin = origin

	    rawset(clip, "normal", normal)
		clip.normal = normal

	    rawset(clip, "localentid", localentid)
		clip.localentid = localentid

		add_queue( clip_queue, rawget(Holo, "e2owner"), { Holo,
			{
				index = idx,
				origin = origin,
				normal = normal,
				localentid = localentid
			}}
		)
	end
end

local function set_visible(Holo, players, visible)
	if not Holo.visible then Holo.visible = {} end
	visible = (visible == 1) or (visible == true)

	local plyCount = #players
	for i = 1, plyCount do
        local ply = rawget(players, i)
        local plyVisibility = rawget(rawget(Holo, "visible"), ply)

		if IsValid( ply ) and ply:IsPlayer() and plyVisibility ~= visible then
		    rawset(rawget(Holo, "visible"), ply, visible)
			add_queue( vis_queue, ply, { Holo, visible } )
		end
	end
end

local function reset_clholo(Holo, scale)

	local holoClips = rawget(Holo, "clips")

	if holoClips then
		for cidx, clip in pairs(holoClips) do
			if rawget(clip, "enabled") then
				add_queue(clip_queue, rawget(Holo, "e2owner"), { Holo,
					{
						index = cidx,
						enabled = false
					}}
				)
			end
		end
		rawset(Holo, "clips", {})
	end

	rescale(Holo, scale, {})
	local holoVisible = rawget(Holo, "visible")

	if holoVisible then
		for ply, state in pairs(holoVisible) do
			if not state then
				add_queue(vis_queue, ply, { Holo, true })
			end
		end

		rawset(Holo, "visible", {})
	end
end

local function set_player_color(Holo, color)
	add_queue(player_color_queue, rawget(Holo, "e2owner"), { Holo, color })
end

hook.Add( "PlayerInitialSpawn", "wire_holograms_set_vars", function(ply)
	local s_queue = {}
	local b_s_queue = {}
	local c_queue = {}

	for pl_uid, rep in pairs( E2HoloRepo ) do

		for k, Holo in pairs( rep ) do

			if Holo and IsValid(Holo.ent) then
				local clips = rawget(Holo, "clips")
				local scale = rawget(Holo, "scale")
				local bone_scales = rawget(Holo, "bone_scale")

				table.insert(s_queue, { Holo, scale })

				if bone_scales and next(bone_scales) ~= nil then
					for bidx,b_scale in pairs(bone_scales) do
						tableInsert(b_s_queue, { Holo, bidx, b_scale })
					end
				end

				if clips and next(clips) ~= nil then
					for cidx, clip in pairs(clips) do
						if rawget(clip, "enabled") then
							tableInsert(c_queue, {
								Holo,
								{
									index = cidx,
									enabled = clip.enabled
								}
							} )
						end

						if rawget(clip, "origin") and rawget(clip, "normal") and rawget(clip, "localentid") then
							tableInsert(c_queue, {
								Holo,
								{
									index = cidx,
									origin = rawget(clip, "origin"),
									normal = rawget(clip, "normal"),
									localentid = rawget(clip, "localentid")
								}
							} )
						end
					end
				end
			end
		end
	end

	flush_scale_queue({[ply] = s_queue}, ply)
	flush_bone_scale_queue({[ply] = b_s_queue}, ply)
	flush_clip_queue({[ply] = c_queue}, ply)
end)

-- -----------------------------------------------------------------------------

local function MakeHolo(Player, Pos, Ang, model)
	local prop = ents.Create( "gmod_wire_hologram" )
	rawget(WireLib, "setPos")(prop, Pos)
	rawget(WireLib, "setAng")(prop, Ang)

	prop:SetModel(model)
	prop:SetPlayer(Player)
	prop:SetNWInt(ownerid, Player:UserID())

	return prop
end

-- Returns the hologram with the given index or nil if it doesn't exist.
local function CheckIndex(self, index)
	index = index - index % 1
	local Holo

	if index<0 then
		local globalHolos = rawget(E2HoloRepo, rawget(self, "uid"))
		Holo = rawget(globalHolos, -index)
	else
		local data = rawget(self, "data")
		local holos = rawget(data, "holos")
		Holo = rawget(holos, index)
	end

	if not Holo or not IsValid(rawget(Holo, "ent")) then return nil end

	return Holo
end

-- Sets the given index to the given hologram.
local function SetIndex(self, index, Holo)
	index = index - index % 1

	local rep = rawget(E2HoloRepo, rawget(self, "uid"))

	if index < 0 then
		rawset(rep, -index, Holo)
	else
		local data = rawget(self, "data")
		local holos = rawget(data, "holos")

		if rawget(holos, index) then
		    local theHolo = rawget(holos, index)
		    rawset(rep, theHolo, nil)
		end

		rawset(holos, index, Holo)
		if Holo then rawset(rep, Holo, Holo) end
	end
end

local function CreateHolo(self, index, pos, scale, ang, color, model)
    local ent = rawget(Self, "entity")

	if not pos   then pos   = rawget(ent, "GetPos")(ent) end
	if not scale then scale = Vector(1,1,1) end
	if not ang   then ang   = rawget(ent, "GetAngles")(ent) end

	model = GetModel(self, model or "cube") or "models/holograms/cube.mdl"

	local Holo = CheckIndex(self, index)
	if not Holo then
		Holo = {}
		SetIndex(self, index, Holo)
	end

	local prop
	local holoEnt = rawget(Holo, "ent")

	if IsValid(holoEnt) then
		prop = holoEnt
		rawget(WireLib, "setPos")(prop, pos)
		rawget(WireLib, "setAng")(prop, ang)
		prop:SetModel( model )
	else
		prop = MakeHolo(rawget(self, "player"), pos, ang, model, {}, {})

		rawget(prop, "Activate")(prop)
		rawget(prop, "Spawn")(prop)
		rawget(prop, "SetSolid")(prop, SOLID_NONE)
		rawget(prop, "SetMoveType")(prop, MOVETYPE_NONE)

		do
            local uid = rawget(self, "uid")
            local new = rawget(PlayerAmount, uid) + 1
            rawset(PlayerAmount, uid, new)
        end

		rawset(Holo, "ent", prop)
		rawset(Holo, "e2owner", self)

		prop:CallOnRemove( "holo_cleanup", function( ent, self, index ) --Give the player more holograms if we get removed
			local Holo = CheckIndex( self, index )
			if not Holo then return end

            local uid = rawget(self, "uid")
            local new = rawget(PlayerAmount, uid) - 1
            rawset(PlayerAmount, uid, new)

			SetIndex( self, index, nil )
		end, self, index )
	end

	if not IsValid(prop) then return nil end

	if color then
        local r = rawget(color, 1)
        local g = rawget(color, 2)
        local b = rawget(color, 3)
        local a = rawget(color, 4) or 255
        local holoColor = Color(r, g, b, a)
        local setColor = rawget(WireLib, "SetColor")

        setColor(rawget(Holo, "ent"), holoColor)
	end

	reset_clholo(Holo, scale) -- Reset scale, clips, and visible status

	rawset(prop, "E2HoloData", Holo)

	return prop
end

-- -----------------------------------------------------------------------------

local function CheckSpawnTimer( self, readonly )
	local holoData = rawget(self, "data")
	local holo = rawget(holoData, "holo")

	local rightNow = CurTime()
	local burstDelay = rawget(wire_holograms_burst_delay, "GetInt")(wire_holograms_burst_delay)
	local burstAmount = rawget(wire_holograms_burst_amount, "GetInt")(wire_holograms_burst_amount)
	local spawnAmount = rawget(wire_holograms_spawn_amount, "GetInt")(wire_holograms_spawn_amount)

	if rightNow >= rawget(holo, "nextSpawn") then
		rawset(holo, "nextSpawn", rightNow + 1)

		if rightNow >= rawget(holo, "nextBurst") then
			rawset(holo, "remainingSpawns", burstAmount)
		elseif rawget(holo, "remainingSpawns") < spawnAmount then
		    rawset(holo, "remainingSpawns", spawnAmount)
		end
	end

	if rightNow >= rawget(holo, "nextBurst") then
	    rawset(holo, "nextBurst", rightNow + burstDelay)
	end

	local holoRemainingSpawns = rawget(holo, "remainingSpawns")

	if holoRemainingSpawns > 0 then
		if not readonly then
		    rawset(holo, "remainingSpawns", holoRemainingSpawns - 1)
		end

		return true
    end

    return false
end

-- Removes all holograms from the given chip.
local function clearholos(self)
	-- delete local holos
	local selfData = rawget(self, "data")
	local holos = rawget(selfData, "holos")

	-- FIXME: Can this use ipairs?
	for index,Holo in ipairs(holos) do remove_holo(Holo) end

	-- delete global holos owned by this chip
	local uid = rawget(self, "uid")
	local rep = rawget(E2HoloRepo, uid)

	if not rep then return end

	local repCount = #rep
	for i = 1, repCount do
	    local Holo = rawget(rep, i)

		if rawget(Holo, "e2owner") == self then
		    remove_holo(Holo)
		end
	end
end

local function clearholos_all(plyUid)
	if plyUid == nil then
		for plyUid in pairs(E2HoloRepo) do
		    clearholos_all(pl_uid)
		end

		return
	end

	-- FIXME: Is this table like 1,2,3,4,5?
	local plyHolos = rawget(E2HoloRepo, plyUid)
	local plyHolosCount = #plyHolos

	for k = 1, plyHolosCount do
        local Holo = rawget(plyHolos, k)

		if Holo then
		    local holoEnt = rawget(holo, "ent")

		    if IsValid(holoEnt) then
                rawget(holoEnt, "RemoveCallOnRemove")( holoEnt, "holo_cleanup" )
                remove_holo(Holo)
            end
		end
	end

	rawset(E2HoloRepo, plyUid, {})
	rawset(PlayerAmount, plyUid, 0)
end

-- -----------------------------------------------------------------------------

__e2setcost(30) -- temporary



e2function entity holoCreate(index, vector position, vector scale, angle ang, vector color, string model)
	if not checkOwner(self) then return end

	local uid = rawget(self, "uid")
	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	ang = Angle(
	    rawget(ang, 1),
	    rawget(ang, 2),
	    rawget(ang, 3)
	)

	local ret = CreateHolo(self, index, position, scale, ang, color, model)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index, vector position, vector scale, angle ang, vector4 color, string model)
	if not checkOwner(self) then return end

	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	ang = Angle(
	    rawget(ang, 1),
	    rawget(ang, 2),
	    rawget(ang, 3)
	)

	local ret = CreateHolo(self, index, position, scale, ang, color, model)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index, vector position, vector scale, angle ang, vector color)
	if not checkOwner(self) then return end

	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	ang = Angle(
	    rawget(ang, 1),
	    rawget(ang, 2),
	    rawget(ang, 3)
	)

	local ret = CreateHolo(self, index, position, scale, ang, color)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index, vector position, vector scale, angle ang, vector4 color)
	if not checkOwner(self) then return end

	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	ang = Angle(
	    rawget(ang, 1),
	    rawget(ang, 2),
	    rawget(ang, 3)
	)

	local ret = CreateHolo(self, index, position, scale, ang, color)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index, vector position, vector scale, angle ang)
	if not checkOwner(self) then return end

	local uid = rawget(self, "uid")
	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	ang = Angle(
	    rawget(ang, 1),
	    rawget(ang, 2),
	    rawget(ang, 3)
	)
	local ret = CreateHolo(self, index, position, scale, ang)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index, vector position, vector scale)
	if not checkOwner(self) then return end

	local uid = rawget(self, "uid")
	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	local ret = CreateHolo(self, index, position, scale)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index, vector position)
	if not checkOwner(self) then return end

	local uid = rawget(self, "uid")
	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	position = Vector(
	    rawget(position, 1),
	    rawget(position, 2),
	    rawget(position, 3)
	)

	local ret = CreateHolo(self, index, position)
	if IsValid(ret) then return ret end
end

e2function entity holoCreate(index)
	if not checkOwner(self) then return end

	local uid = rawget(self, "uid")
	local ply = rawget(self, "player")
	local isBlocked = rawget(BlockList, rawget(ply, "SteamID")(ply))

	if isBlocked == true or CheckSpawnTimer( self ) == false then return end

	local Holo = CheckIndex(self, index)

	local overLimit = rawget(PlayerAmount, uid) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
	if not Holo and overLimit then return end

	local ret = CreateHolo(self, index)
	if IsValid(ret) then return ret end
end

__e2setcost(20)
e2function void holoDelete(index)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	remove_holo(Holo)
end

e2function void holoDeleteAll()
	clearholos(self)
end

e2function void holoDeleteAll( all )
	if all > 0 then
		clearholos_all( rawget(self, "uid") )
	else
		clearholos( self )
	end
end

e2function void holoReset(index, string model, vector scale, vector color, string material)
	model = GetModel(self, model)
	if not model then return end

	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local holoEnt = rawget(Holo, "ent")

	holoEnt:SetModel(model)

	local setColor = rawget(WireLib, "SetColor")
	local col = Color(
	    rawget(color, 1),
	    rawget(color, 2),
	    rawget(color, 3),
	    255
	)

	setColor(holoEnt, col)

	rawget(E2Lib, "setMaterial")(holoEnt, material)

	remove_from_queues( holoEnt )
	reset_clholo(Holo, scale) -- Reset scale, clips, and visible status
end

__e2setcost(2)

e2function number holoCanCreate()
	if (not checkOwner(self)) then return 0 end

	if CheckSpawnTimer(self, true) == false or rawget(PlayerAmount, rawget(self, "uid")) >= rawget(wire_holograms_max, "GetInt")(wire_holograms_max) then
		return 0
	end

	return 1
end

e2function number holoRemainingSpawns()
	CheckSpawnTimer(self, true)
	local selfData = rawget(self, "data")
	local dataHolo = rawget(selfData, "holo")
	
	return rawget(dataHolo, "remainingSpawns")
end

e2function number holoAmount()
	return rawget(PlayerAmount, rawget(self, "uid"))
end

e2function number holoMaxAmount()
	return rawget(wire_holograms_max, "GetInt")(wire_holograms_max)
end

-- -----------------------------------------------------------------------------

__e2setcost(15) -- temporary

e2function void holoScale(index, vector scale)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rescale(Holo, scale)
end

e2function vector holoScale(index)
	local Holo = CheckIndex(self, index)
	if not Holo then return {0,0,0} end

	return Holo.scale or {0,0,0} -- TODO: maybe {1,1,1}?
end

e2function void holoScaleUnits(index, vector size)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local holoEnt = rawget(Holo, "ent")
	local propsize = rawget(holoEnt,"OBBMaxs")(holoEnt) - rawget(holoEnt, "OBBMins")(holoEnt)

	local x = rawget(size, 1) / rawget(propsize, "x")
	local y = rawget(size, 2) / rawget(propsize, "y")
	local z = rawget(size, 3) / rawget(propsize, "z")

	rescale(Holo, Vector(x, y, z))
end

e2function vector holoScaleUnits(index)
	local Holo = CheckIndex(self, index)
	if not Holo then return {0,0,0} end

	local scale = rawget(Holo, "scale") or {0,0,0} -- TODO: maybe {1,1,1}?

	local holoEnt = rawget(Holo, "ent")
	local propsize = rawget(holoEnt, "OBBMaxs")(holoEnt) - rawget(holoEnt, "OBBMins")(holoEnt)

	return Vector(
        rawget(scale, 1) * rawget(propsize, "x"),
        rawget(scale, 2) * rawget(propsize, "y"),
        rawget(scale, 3) * rawget(propsize, "z")
    )
end


e2function void holoBoneScale(index, boneindex, vector scale)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rescale(Holo, nil, {boneindex, scale})
end

e2function void holoBoneScale(index, string bone, vector scale)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local boneindex = rawget(Holo, "ent"):LookupBone(bone)
	if boneindex == nil then return end

	rescale(Holo, nil, {boneindex, scale})
end

e2function vector holoBoneScale(index, boneindex)
	local default = {0, 0, 0}
	local Holo = CheckIndex(self, index)
	if not Holo then return default end

	local boneScale = rawget(Holo, "bone_scale")
	return rawget(boneScale, boneIndex) or default
end

e2function vector holoBoneScale(index, string bone)
	local default = {0, 0, 0}
	local Holo = CheckIndex(self, index)
	if not Holo then return {0,0,0} end

	local boneindex = rawget(Holo, "ent"):LookupBone(bone)
	if boneindex == nil then return default end

	local boneScale = rawget(Holo, "bone_scale")
	return rawget(boneScale, boneIndex) or default
end
__e2setcost(1)
e2function number holoClipsAvailable()
	return wire_holograms_max_clips:GetInt()
end

__e2setcost(15)
e2function void holoClipEnabled(index, enabled) -- Clip at first index
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	if enabled == 1 then
		enable_clip(Holo, 1, true)
	elseif enabled == 0 then
		enable_clip(Holo, 1, false)
	end
end

e2function void holoClipEnabled(index, clipidx, enabled)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	if enabled == 1 then
		enable_clip(Holo, clipidx, true)
	elseif enabled == 0 then
		enable_clip(Holo, clipidx, false)
	end
end

e2function void holoClip(index, vector origin, vector normal, isglobal) -- Clip at first index
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	set_clip(
        Holo, 1,
        Vector(
            rawget(origin, 1),
            rawget(origin, 2),
            rawget(origin, 3)
        ),
        Vector(
            rawget(normal, 1),
            rawget(normal, 2),
            rawget(normal, 3)
        ),
        isglobal ~= 0 and 0 or rawget(Holo, "ent"):EntIndex()
    )
end

e2function void holoClip(index, clipidx, vector origin, vector normal, isglobal)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	set_clip(
        Holo, clipidx,
        Vector(
            rawget(origin, 1),
            rawget(origin, 2),
            rawget(origin, 3)
        ),
        Vector(
            rawget(normal, 1),
            rawget(normal, 2),
            rawget(normal, 3)
        ),
        isglobal ~= 0 and 0 or rawget(Holo, "ent"):EntIndex()
    )
end

e2function void holoClip(index, vector origin, vector normal, entity localent) -- Clip at first index
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	set_clip(
        Holo, 1,
        Vector(
            rawget(origin, 1),
            rawget(origin, 2),
            rawget(origin, 3)
        ),
        Vector(
            rawget(normal, 1),
            rawget(normal, 2),
            rawget(normal, 3)
        ),
        localent:EntIndex()
    )
end

e2function void holoClip(index, clipidx, vector origin, vector normal, entity localent)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	set_clip(
        Holo, clipidx,
        Vector(
            rawget(origin, 1),
            rawget(origin, 2),
            rawget(origin, 3)
        ),
        Vector(
            rawget(normal, 1),
            rawget(normal, 2),
            rawget(normal, 3)
        ),
        localent:EntIndex()
    )
end

e2function void holoPos(index, vector position)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(WireLib, "setPos")(
	    rawget(Holo, "ent"),
	    Vector(
	        rawget(position, 1),
	        rawget(position, 2),
	        rawget(position, 3)
	    )
	)
end

e2function void holoAng(index, angle ang)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(WireLib, "setAng")(
	    rawget(Holo, "ent"),
	    Angle(
	        rawget(ang, 1),
	        rawget(ang, 2),
	        rawget(ang, 3)
	    )
	)
end

-- -----------------------------------------------------------------------------

e2function void holoColor(index, vector color)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local holoEnt = rawget(Holo, "ent")

	rawget(WireLib, "SetColor")(
	    holoEnt,
	    Color(
	        rawget(color, 1),
	        rawget(color, 2),
	        rawget(color, 3),
	        rawget(holoEnt:GetColor(), "a")
	    )
	)
end

e2function void holoColor(index, vector4 color)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(WireLib, "SetColor")(
        rawget(Holo, "ent"),
        Color(
            rawget(color, 1),
            rawget(color, 2),
            rawget(color, 3),
            rawget(color, 4)
        )
    )
end

e2function void holoColor(index, vector color, alpha)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(WireLib, "SetColor")(
	    rawget(Holo, "ent"),
	    Color(
	        rawget(color, 1),
	        rawget(color, 2),
	        rawget(color, 3),
	        alpha
	    )
	)
end

e2function void holoAlpha(index, alpha)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local holoEnt = rawget(Holo, "ent")

	local c = holoEnt:GetColor()
	rawset(c, a, alpha)

	rawget(WireLib, "SetColor")(holoEnt, c)
end

__e2setcost(10)
e2function void holoShadow(index, has_shadow)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(Holo, "ent"):DrawShadow( has_shadow ~= 0 )
end

e2function void holoDisableShading( index, disable )
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(Holo, "ent"):SetNWBool( "disable_shading", disable == 1 )
end

-- -----------------------------------------------------------------------------

e2function array holoModelList()
	local mlist = {}

	for k in pairs( ModelList ) do
	    table.insert(mlist, k)
	end

	return mlist
end

__e2setcost(1)
e2function number holoModelAny()
	return wire_holograms_modelany:GetInt()
end

__e2setcost(10)
e2function void holoModel(index, string model)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	model = GetModel(self, model)
	if not model then return end

	local holoEnt = rawget(Holo, "ent)")
	rawget(holoEnt, "SetModel")(holoEnt, model)
end

e2function void holoModel(index, string model, skin)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	skin = skin - skin % 1

	model, skin = GetModel(self, model, skin)
	if not model then return end

	local holoEnt = rawget(Holo, "ent")
	rawget(holoEnt, "SetModel")(holoEnt, model)
	rawget(holoEnt, "SetSkin")(holoEnt, skin)
end

e2function void holoSkin(index, skin)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local holoEnt = rawget(Holo, "ent")

	skin = skin - skin % 1
	local _, skin = GetModel(self, rawaget(holoEnt, "GetModel")(holoEnt), skin)

	rawget(holoEnt, "SetSkin")(holoEnt, skin)
end

e2function void holoMaterial(index, string material)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(E2Lib, "setMaterial")(rawget(Holo, "ent"), material)
end

e2function void holoPlayerColor(index, vector color)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local r = rawget(color, 1) / 255
	local g = rawget(color, 2) / 255
	local b = rawget(color, 3) / 255

	set_player_color(Holo, Vector(r, g, b))
end

e2function void holoRenderFX(index, effect)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	effect = effect - effect % 1
	rawget(Holo, "ent"):SetKeyValue("renderfx",effect)
end

e2function void holoBodygroup(index, bgrp_id, bgrp_subid)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	rawget(Holo, "ent"):SetBodygroup(bgrp_id, bgrp_subid)
end

e2function number holoBodygroups(index, bgrp_id)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	return rawget(Holo, "ent"):GetBodygroupCount(bgrp_id)
end

-- -----------------------------------------------------------------------------

e2function void holoVisible(index, entity ply, visible)
	local Holo = CheckIndex(self, index)
	if not Holo or not IsValid( ply ) or not ply:IsPlayer() then return end

	set_visible(Holo, { ply }, visible)
end

e2function void holoVisible(index, array players, visible)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	set_visible(Holo, players, visible)
end

-- -----------------------------------------------------------------------------
local function Parent_Hologram(holo, ent, attachment)
    local holoEnt = rawget(holo, "ent")

    local parent = rawget(ent, "GetParent")(ent)
	if parent and rawget(parent, "IsValid")(parent) and parent == rawget(holo, "ent") then return end

	rawget(holoEnt, "SetParent")(holoEnt, ent)

	if attachment ~= nil then
		rawget(holoEnt, "Fire")(holoEnt, "SetParentAttachmentMaintainOffset", attachment, 0.01)
	end
end

-- Check for recursive parenting
local function Check_Parents(child, parent)
    local getParent = rawget(parent, "GetParent")

	while true do
		parent = getParent( parent )
		if not IsValid( parent ) then break end

		if parent == child then
			return false
		end
	end

	return true
end

__e2setcost(40)
e2function void holoParent(index, otherindex)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local Holo2 = CheckIndex(self, otherindex)
	if not Holo2 then return end

	local holoEnt = rawget(Holo, "ent")
	local holo2Ent = rawget(Holo2, "ent")

	if not Check_Parents(holoEnt, holo2Ent) then return end

	Parent_Hologram(Holo, holo2Ent, nil)
end

e2function void holoParent(index, entity ent)
	if not IsValid(ent) then return end
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	if not Check_Parents(rawget(Holo, "ent"), ent) then return end

	Parent_Hologram(Holo, ent, nil)
end

e2function void holoParentAttachment(index, entity ent, string attachmentName)
	if not IsValid(ent) then return end
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	Parent_Hologram(Holo, ent, attachmentName)
end

e2function void holoUnparent(index)
	local Holo = CheckIndex(self, index)
	if not Holo then return end

	local holoEnt = rawget(Holo, "ent")
	rawget(holoEnt, "SetParent")(holoEnt, nil)
	rawget(holoEnt, "SetParentPhysNum")(holoEnt, 0)
end

-- -----------------------------------------------------------------------------

__e2setcost(2)
e2function entity holoEntity(index)
	local Holo = CheckIndex(self, index)
	local holoEnt = rawget(Holo, "ent")
	if Holo and IsValid(holoEnt) then return holoEnt end
end

__e2setcost(30)
--- Gets the hologram index of the given entity, if any. Returns 0 on failure.
e2function number holoIndex(entity ent)
	if not IsValid(ent) then return 0 end
	if ent:GetClass() ~= "gmod_wire_hologram" then return 0 end

	-- check local holos
	local selfData = rawget(self, "data")
	local dataHolos = rawget(selfData, "holos")
	for k, Holo in pairs(dataHolos) do
		if(ent == rawget(Holo, "ent")) then return k end
	end

	-- check global holos
	for k, Holo in pairs(rawget(E2HoloRepo, rawget(self, "uid"))) do
		if isnumber(k) and ent == rawget(Holo, "ent") then return -k end
	end

	return 0
end

-- -----------------------------------------------------------------------------

registerCallback("construct", function(self)
    local uid = rawget(self, "uid")
    local holoRepo = rawget(E2HoloRepo, uid)

	if not holoRepo then
		rawset(E2HoloRepo, uid, {})
		rawset(PlayerAmount, uid, 0)
	end

	local rightNow = CurTime()

	local selfData = rawget(self, "data")
	rawset(selfData, "holos", {})
	rawset(selfData, "holo", {
		nextSpawn = rightNow + 1,
		nextBurst = rightNow + wire_holograms_burst_delay:GetInt(),
		remainingSpawns = wire_holograms_burst_amount:GetInt()

	})
end)

registerCallback("destruct", function(self)
	clearholos(self)
end)

-- -----------------------------------------------------------------------------

local function ConsoleMessage(ply, text)
	if rawget(ply, "IsValid")(ply) then
		rawget(ply, "PrintMessage")( ply, HUD_PRINTCONSOLE, text )
	else
		print(text)
	end
end

concommand.Add( "wire_holograms_remove_all", function( ply, com, args )
	if rawget(ply, "IsValid")(ply) and not rawget(ply, "IsAdmin")(ply) then return end

	clearholos_all()
end )

concommand.Add( "wire_holograms_block", function( ply, com, args )
	if rawget(ply, "IsValid")(ply) and not rawget(ply, "IsAdmin")(ply) then return end

	local firstArg = rawget(args, 1)

	if not firstArg then
		ConsoleMessage( ply, "Command requires a player's name (or part of their name)" )
		ConsoleMessage( ply, "Usage: wire_holograms_block [name]" )
		return
	end

	local name = rawget(firstArg, "lower")(firstArg)
	local players = E2Lib.filterList(player.GetAll(), function(ent) return ent:GetName():lower():match(name) end)

	if #players == 1 then
		local v = players[1]
		if BlockList[v:SteamID()] == true then
			ConsoleMessage( ply, v:GetName() .. " is already in the holograms blocklist!" )
		else
			local uid = v:UniqueID()
			if E2HoloRepo[uid] then
				clearholos_all(uid)
			end
			BlockList[v:SteamID()] = true
			for _,p in ipairs( player.GetAll() ) do
				p:PrintMessage( HUD_PRINTTALK, "(ADMIN) " .. v:GetName() .. " added to holograms blocklist" )
			end
		end
	elseif #players > 1 then
		ConsoleMessage( ply, "More than one player matches that name!" )
	else
		ConsoleMessage( ply, "No player names found with " .. args[1] )
	end
end )

concommand.Add( "wire_holograms_unblock", function( ply, com, args )
	if ply:IsValid() and not ply:IsAdmin() then return end

	if not args[1] then
		ConsoleMessage( ply, "Command requires a player's name (or part of their name)" )
		ConsoleMessage( ply, "Usage: wire_holograms_unblock [name]" )
		return
	end

	local name = args[1]:lower()
	local players = E2Lib.filterList(player.GetAll(), function(ent) return ent:GetName():lower():match(name) end)

	if #players == 1 then
		local v = players[1]
		if BlockList[v:SteamID()] == true then
			BlockList[v:SteamID()] = nil
			for _,player in ipairs( player.GetAll() ) do
				player:PrintMessage( HUD_PRINTTALK, "(ADMIN) " .. v:GetName() .. " removed from holograms blocklist" )
			end
		else
			ConsoleMessage( ply, v:GetName() .. " is not in the holograms blocklist!" )
		end
	elseif #players > 1 then
		ConsoleMessage( ply, "More than one player matches that name!" )
	else
		ConsoleMessage( ply, "No player names found with " .. args[1] )
	end
end )

concommand.Add( "wire_holograms_block_id", function( ply, com, args )
	if ply:IsValid() and not ply:IsAdmin() then return end

	local steamID = table.concat(args)

	if not steamID:match("STEAM_[0-9]:[0-9]:[0-9]+") then
		ConsoleMessage( ply, "Invalid SteamID format" )
		ConsoleMessage( ply, "Usage: wire_holograms_block_id STEAM_X:X:XXXXXX" )
		return
	end

	if BlockList[steamID] == true then
		ConsoleMessage( ply, steamID .. " is already in the holograms blocklist!" )
	else
		BlockList[steamID] = true
		for _,player in ipairs( player.GetAll() ) do
			player:PrintMessage( HUD_PRINTTALK, "(ADMIN) " .. steamID .. " added to holograms blocklist" )
		end
		local uid
		for _,v in pairs( player.GetAll() ) do
			if v:SteamID() == steamID then
				uid = v:UniqueID()
				if (E2HoloRepo[uid]) then
					clearholos_all(uid)
					return
				end
			end
		end
	end
end )

concommand.Add( "wire_holograms_unblock_id", function( ply, com, args )
	if ply:IsValid() and not ply:IsAdmin() then return end

	local steamID = table.concat(args)

	if not steamID:match("STEAM_[0-9]:[0-9]:[0-9]+") then
		ConsoleMessage( ply, "Invalid SteamID format" )
		ConsoleMessage( ply, "Usage: wire_holograms_unblock_id STEAM_X:X:XXXXXX" )
		return
	end

	if BlockList[steamID] == true then
		BlockList[steamID] = nil
		for _,player in ipairs( player.GetAll() ) do
			player:PrintMessage( HUD_PRINTTALK, "(ADMIN) " .. steamID .. " removed from holograms blocklist" )
		end
	else
		ConsoleMessage( ply, steamID .. " is not in the holograms blocklist!" )
	end
end )

-- -----------------------------------------------------------------------------

wire_holograms = {} -- This global table is used to share certain functions and variables with UWSVN
wire_holograms.CheckIndex = CheckIndex

registerCallback( "postinit", function()
	timer.Simple( 1, function()
		wire_holograms = nil
	end )
end )
